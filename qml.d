/**
 * qml.d - compile-time binding generator for D/qml prjects
 *
 * qml.d is one of two parts in a system to facilitate projects using both
 * qml and D. The other part is a proof of concept build system, qdmd.
 *
 * Classes exported with ExternQML can be used imported and used in qml files.
 * qml acts primarily the controller and interacts with D by calling D
 * functions and properties. D code can communicate back to qml through the use
 * of Qt signals. See below.
 *
 * Usage
 * =====
 *  1. Below classes with an 'extern(C++)' decorator, add a line similar to:
 *     mixin(ExternQML!("modulename", "classname", versionMajor, versionMinor));
 *  2. Call qdmd_main() somewhere.
 *  3. Compile your project with 'rdmd qdmd.d *qml *d'
 *
 * Quick Reference Table
 * =====================
 *
 * | In D use                             | In QML use                                |
 * | ------------------------------------ | ----------------------------------------- |
 * | @qtSignal void heightChanged(int);   | onHeightChanged: { console.log(height); } |
 * | @property int height();              | x.height;                                 |
 * | @property void height(int);          | x.height = 2;                             |
 * | @qtSignal xChanged _and_ @property x | someOtherProperty: x * 2                  |
 * | string ((immutable char)[])          | string (QString-based)                    |
 *
 * Note: Until I figure out how to do things differently, signals must have c linking
 * on Windows (i.e., prefix them with 'extern(C)')
 *
 * Note: All arguments must be named.
 *
 * Note: Signals are to be declared, but not implemented. The implementation is done for
 * you.
 *
 * Copyright
 * =========
 * Copyright (C) 2014 Josh Netterfield <joshua@netek.ca>
 * This work is free. You can redistribute it and/or modify it under the
 * terms of the Do What The Fuck You Want To Public License, Version 2,
 * as published by Sam Hocevar.  See http://www.wtfpl.net/ for more details.
 */
module qdmd.qml;

import std.algorithm;
import std.array;
import std.conv;
import std.functional;
import std.json;
import std.process;
import std.range;
import std.regex;
import std.stdio;
import std.string;
import std.traits;

/**
 * Utility to transform the following into a JSONValue:
 *  - 'short's
 *  - 'ushort's
 *  - 'int's
 *  - 'long's
 *  - 'uint's'
 *  - 'ulong's
 *  - 'float's
 *  - 'string's
 *  - 'bool's
 *  - static and dynamic arrays with any of these types
 *  - iterables with any of these types with to!string()-able keys
 *  - data from classes or structs with only these types
 */
JSONValue jsonify(T)(T t) {
    import std.traits;

    JSONValue ret;
    static if (is(T == string)) {
        ret.str = t;
        return ret;
    } else static if (is(T == int) || is(T == long) || is(T == short) || is(T == ushort)) {
        ret.integer = t;
        return ret;
    } else static if (is(T == uint) || is(T == ulong)) {
        ret.uinteger = t;
        return ret;
    } else static if (is(T == float) || is(T == double)) {
        ret.type = JSON_TYPE.FLOAT;
        ret.floating = t;
        return ret;
    } else static if (is(T == bool)) {
        if (t) {
            ret.type = JSON_TYPE.TRUE;
        } else {
            ret.type = JSON_TYPE.FALSE;
        }
        return ret;
    } else static if (isStaticArray!(T) || isDynamicArray!(T)) {
        JSONValue arr[];
        foreach (v; t) {
            arr ~= jsonify(v);
        }
        ret.array = arr;
        return ret;
    } else static if (isIterable!(T)) {
        ret.type = JSON_TYPE.OBJECT;
        JSONValue[string] reto;

        foreach (k, v; t) {
            reto[k.to!string()] = jsonify(v);
        }
        ret.object = reto;
        return ret;
    } else {
        JSONValue[string] reto;

        enum members = [ __traits(allMembers, T) ];
        foreach (I, TYPE; typeof(T.tupleof)) {
            static if(__traits(compiles, __traits(getMember, t, __traits(identifier, T.tupleof[I])))) {
                auto val =
                    __traits(getMember, t, __traits(identifier, T.tupleof[I]));

                reto[__traits(identifier, T.tupleof[I])] =
                    jsonify(val);
            }
        }
        reto["__type__"] = jsonify(T.stringof);
        JSONValue j = reto;
        return j;
    }
}

string stringify(T)(T t) {
    auto jv = t.jsonify;
    return (&jv).toJSON();
}

void renderFile(string filename)() {
    enum qml = import(filename);
    render!(qml, filename);
}

void render(string str)() {
    render!(str, "main.qml")();
}

// For some reason, join freezes on DMD32.064 on Windows 
string ctfeJoin(T)(T s, string sep) {
    string ret;
    foreach(c; s) {
        ret ~= c ~ sep;
    }
    return ret;
}

// Join, filtering all non-alphabetical characters
string ctfeJoinAlpha(T)(T s) {
    string ret;
    foreach(c; s) {
        if (c.isAlpha) {
            ret ~= c;
        }
    }
    return ret;
}

string findLastDropOne(T, U)(T s, U m) {
    string ret;
    foreach(c; s) {
        if (c == m) {
            ret = "";
        } else {
            ret ~= c;
        }
    }
    return ret;
}

void render(string str, string filename)() {
    enum lines = str
        .split("\n")
        .filter!(line => line.strip.startsWith("import"))
        .map!(line => line.replace("import ", "").retro.find!(a => a == ' ' || a.isAlpha).retro.ctfeJoinAlpha)
        .array;
    pragma(msg, "@qdmd.rcc.import@ ", lines);
    if (!__ctfe) {
        "Madness begins".writeln();
        qdmd_main(str);
    }
}

bool isCompatiblePrimitive(rt)() {
    return
        is(rt == void) ||
        is(rt == string) ||
        is(rt == int) ||
        is(rt == long) ||
        is(rt == ulong) ||
        is(rt == float) ||
        is(rt == double) ||
        is(rt == bool);
}

string declare(string manifest)() {
    enum parts = manifest.split(" ");

    assert(parts.length == 2, "qml.exportClass expects a string like: <module>.<symbol> <majorVersion>.<minorVersion>");
    assert(parts[0].split(".").length >= 2, "qml.exportCall exports a module and symbol: <module>.<symbol> <majorVersion>.<minorVersion>");
    assert(parts[1].split(".").length == 2, "Version syntax: <majorVersion>.<minorVersion>");

    enum module_ = parts[0].retro.find(".").retro.dropBackOne;
    enum T = parts[0].findLastDropOne('.');
    enum major = parts[1].split(".")[0].to!int;
    enum minor = parts[1].split(".")[1].to!int;

    mixin("import " ~ module_ ~ ";");
    assert(__ctfe, "qml.exportClass should only be run in a mixin at file scope. It should not executed at runtime.");
    assert(moduleName!(mixin(T)) == module_);

    string dToC(string type, bool isReturn = false, bool isJson = false) {
        if (isJson && isReturn) {
            return "const char*";
        }
        if (type == "const(char*)") {
            return "const char*";
	}
        if (type == "string") {
            // See note on eax/edx below.
            return "int";
        }
        return type;
    }
    string dToQt(string type, bool isReturn = false, bool isJson = false) {
        if (isReturn && isJson) {
            return "QJsonValue";
        }
        if (type == "string") {
            return "QString";
        }
	if (type == "const(char*)") {
            return "const char*";
	}
        return type;
    }

    /// Yuck!
    struct Identifier {
        string name;
        string type;
        string protection;
        string[] arguments;
        string[] argNames;
        bool isProperty = false;
        bool isSignal = false;
        bool isJson = false;
        string mangledName;

        invariant() {
            assert(this.name.length);
        }

        string cInterface() const {
            string[] ax = ax();
            return "virtual " ~ dToC(this.type, true, isJson) ~ " " ~ this.name ~ "(" ~ ax.join(", ") ~ ");";
        }

        string[] ax() const {
            string[] ax;
            foreach(idx, a; arguments) {
                auto symbolName = argNames[idx];
                ax ~= dToC(a, false, isJson) ~ " " ~ symbolName;
                if (a == "string") {
                    ax ~= "char* " ~ symbolName ~ "_content";
                }
            }
            return ax;
        }
        /**
          * C implementation of functions in the base class (i.e., not the shim).
          *
          * Currently, only signals need implementations.
          */
        string cImpl(string mod, string cName) const {
            string[] ax = this.ax();
            string[] bx;
            foreach(idx, a; arguments) {
                auto symbolName = argNames[idx];
                if (a == "string") {
                    bx ~= "QString::fromLocal8Bit(" ~ symbolName ~ "_content, " ~ symbolName ~ ")";
                } else {
                    bx ~= symbolName;
                }
            }
            if (this.isSignal) {
                return "extern \\\"C\\\" void " ~ this.mangledName ~
                    "(" ~ (T ~ "* _this" ~ ax).join(", ") ~ ") {" ~
                    T ~ "Shim* v = " ~ T ~ "Shim::s_map.value(_this);" ~ 
                    "if(v) v->" ~ this.name ~
                    "(" ~ bx.join(", ") ~ ");" ~
                    "}";
            }
            return "";
        }

        /**
          * D implementations of functions used by the shim class.
          * 
          * Currently, thin wrappers around contructors, thin wrappers around destructors, and Jsonification functions.
          */
        string dImpl(string cName) const {
            string[] ax;
            string[] bx;
            foreach(idx, a; arguments) {
                auto symbolName = argNames[idx];
                ax ~= a ~ " " ~ symbolName;
                bx ~= symbolName;
            }
            //if (this.isSignal) {
            //    return "extern(C++) " ~ this.type ~ " _signal_" ~ cName ~ "_" ~ this.name ~
            //        /"(" ~ ("void* shim" ~ ax).join(", ") ~ ");\n" ~
            //        //"class " ~ cName ~ " { " ~ this.type ~ " " ~ this.name ~
            //        "(" ~ ("void* shim" ~ ax).join(", ") ~ ") {\n" ~
            //        "_signal_" ~ cName ~ "_" ~ this.name ~ "(" ~ ("null" ~ bx).join(", ") ~ ");" ~
            //        "}}";
            //}
            return "";
        }

        /**
          * C implementation of the shim class.
          */
        string shim(string T) const {
            string[] ax;
            string[] bx;
            string retStr = type == "void" ? "" : "return ";
            string sStr = "";
            string sStr2 = "";
            foreach(idx, a; arguments) {
                auto symbolName = argNames[idx];
                ax ~= dToQt(a, false, isJson) ~ " " ~ argNames[idx];
                bx ~= symbolName;
                if (a == "string") {
                    string s = bx[bx.length - 1];
                    bx[bx.length - 1] = s ~ ".length(), " ~ s ~ ".toLatin1().data()";
                }
            }
            if (isJson) {
                return "Q_SLOT QJsonValue " ~ this.name ~ "(" ~ ax.join(", ") ~ ") { " ~
                    "QJsonDocument doc = QJsonDocument::fromJson(" ~ jsonName(T) ~ "(" ~ (["m_itfce"] ~ bx).join(", ") ~ "));" ~
                    "if (doc.isObject()) { return doc.object(); }" ~
                    "if (doc.isArray()) { return doc.array(); }" ~
                    "}";
            }
            if (this.isSignal) {
                return "Q_SIGNAL " ~ dToQt(this.type, false, isJson) ~ " " ~ this.name ~ "(" ~ ax.join(", ") ~ ");";
            }
            if (type == "string") {
                // In D, strings are not null terminated. On x86, when passing dynamic arrays,
                // eax holds the size of the array (this lines up nicely with C's ABI - it looks like
                // D is returning an int!). The data itself is captured in edx, which we need to
                // capture! Another possible solution -- at least on x86 windows -- is to compile it
                // with 'long' and reinterpret_cast the top and bottom bits. I'm not sure which
                // is likely to work more often.
                retStr = "";
                sStr = "const char* val; int x = ";
                sStr2 = "asm (\\\"movq %%rdx, %0;\\\" : \\\"=r\\\" ( val )); " ~
                    "return QString::fromLocal8Bit(val, int(x));";
            }
            return "Q_SLOT " ~ dToQt(this.type, true, isJson) ~ " " ~ this.name ~ "(" ~ ax.join(", ") ~ ") {" ~ retStr ~ sStr ~ "m_itfce->" ~ this.name ~ "(" ~ bx.join(", ") ~ "); " ~ sStr2 ~ " }";
        }

        /**
         * The following functions are involved with jsonification.
         */
        string jsonName(string T) const {
            string aj;
            foreach(a; arguments) {
                aj ~= a;    // join is not supported for const arrays in 2.064
            }
            return ("_json_" ~ T ~ "_" ~ type ~ "__" ~ name ~ "__" ~ aj ~ "_conv").replace("*", "ptr").replace("[", "a").replace("]", "rr");
        }

        string jx(string T, bool useDTypes = false) const {
            if (!useDTypes) {
                string ret = ax().join(",");
                if (ret.length) {
                    return "void*," ~ ret;
                } else {
                    return "void*";
                }
            }
            string aj;
            aj = T ~ " t";

            foreach(idx, arg; arguments) {
                aj ~= "," ~ arg ~ " " ~ argNames[idx];
            }
            return aj;
        }

        string cConvPtrTypes(string T) const {
            if (isJson) {
                return "const char* (*" ~ jsonName(T) ~ ")(" ~ jx(T) ~ ");";
            }
            return "";
        }
        string dConvPtrTypes(string T) const {
            if (isJson) {
                return "extern(C++) alias immutable(char)* function(" ~ jx(T, true) ~ ") c_" ~ jsonName(T) ~ ";";
            }
            return "";
        }
        string dConvPtrImpl(string T) const {
            if (isJson) {
                return "extern(C++) immutable(char)* " ~ jsonName(T) ~ "(" ~ jx(T, true) ~ ") { return t." ~ name ~ "(" ~ argNames.ctfeJoin(",") ~ ").stringify.toStringz(); }";
            }
            return "";
        }
        string cConvPtrDecls(string T) const {
            if (isJson) {
                return ", const char* (* c_" ~ jsonName(T) ~ ")(" ~ jx(T) ~ ")";
            }
            return "";
        }
        string dConvPtrDecls(string T) const {
            if (isJson) {
                return ", c_" ~ jsonName(T);
            }
            return "";
        }
        string cConvPtrAsns(string T) const {
            if (isJson) {
                return jsonName(T) ~ " = c_" ~ jsonName(T) ~ ";";
            }
            return "";
        }
        string dConvPtrRefs(string T) const {
            if (isJson) {
                return ", &" ~ jsonName(T);
            }
            return "";
        }

        //identifiers.map!(id => id.dConvPtrRefs(T)).join("").replace("\n", "\n@qdmd.cpp@") ~ ");\n";
    }

    string[] qtProperties(Identifier[] ids) {
        struct P {
            string rType;
            string read;
            string write;
            string signal;
        }
        P props[string];
        foreach(id; ids) {
            if (!id.isProperty && (!id.isSignal || !id.name.endsWith("Changed"))) {
                continue;
            }
            string n = id.name;
            if (n.endsWith("Changed")) {
                n = n.dropBack("Changed".length);
            }
            if (!(n in props)) {
                props[n] = P();
            }
            if (!id.arguments.length) {
                if (!id.isSignal) {
                    props[n].read = n;
                    props[n].rType = dToQt(id.type, true, id.isJson);
                } else {
                    props[n].signal = n ~ "Changed";
                }
            } else if (id.arguments.length == 1) {
                props[n].write = n;
            }
        }
        string[] ret;
        foreach(prop; props) {
            assert (!prop.signal.length || prop.read.length || prop.write.length,
                "Found a signal ending with 'Changed' without matching a property.");
            assert (prop.rType.length,
                "Every property needs a 'read' function.");
            string qProp = "        Q_PROPERTY(" ~ prop.rType ~ " " ~ prop.read ~ " READ " ~ prop.read;
            if (prop.write.length) {
                qProp ~= " WRITE " ~ prop.write;
            }
            if (prop.signal.length) {
                qProp ~= " NOTIFY " ~ prop.signal;
            }
            qProp ~= ");";
            ret ~= qProp;
        }
        return ret;
    }

    Identifier identifiers[];

    foreach (string field; __traits(allMembers, mixin(T))) {
        // This test is needed to avoid failing on private functions.
        static if (__traits(compiles, 
                typeof(__traits(getOverloads, mixin(T), field)))) {

            alias typeof(__traits(getOverloads, mixin(T), field)) types;
            foreach(idx, overload; __traits(getOverloads, mixin(T), field)) {
                enum protection = __traits(getProtection, overload);
                enum attributes = __traits(getAttributes, overload);
                auto tokens = types[idx].stringof.split();

                bool isSignal = 
                    (() {
                        foreach(attrib; attributes) {
                            if (attrib == "qtSignal") {
                                return true;
                            }
                        }
                        return false;
                    }());

                enum isJson = !isCompatiblePrimitive!(ReturnType!overload);

                if (isSignal) {
                    assert(functionLinkage!overload == "C",
                        "Signals should have C linkage. Try 'extern(C) @qtSignal'...");
                } else {
                    assert(functionLinkage!overload == "C++",
                        T ~ " should have C++ linkage. Try 'extern(C++) class " ~ T ~ " { ... }'");
                }

                identifiers ~= Identifier(
                    /* name */
                        field,
                    /* type */
                        ReturnType!overload.stringof,
                    /* protection */
                        protection,
                    /* arguments */
                        (function string[] () {
                            enum params = ParameterIdentifierTuple!overload;
                            static if (params.length) {
                                string[] ret;
                                foreach(p; ParameterTypeTuple!overload) {
                                    ret ~= p.stringof;
                                }
                                return ret;
                            }
                            return [];
                        }()),
                    /* argNames */
                        (function string[] () {
                            enum params = ParameterIdentifierTuple!overload;
                            static if (params.length) {
                                string[] ret;
                                foreach(p; params) {
                                    ret ~= p;
                                }
                                return ret;
                            }
                            return [];
                        }()),
                    /* isProperty */
                        0 != (functionAttributes!overload & FunctionAttribute.property),
                    /* isSignal */
                        isSignal,
                    /* isJsonified */
                        isJson,
                    /* mangledName */
                        mangledName!overload
                );
            }
        }
    }

    string cpp, d;

    cpp ~= "@qdmd.cpp.begin@ qdmd_" ~ T ~ ".hpp\n";

    // TODO(joshnetterfield): Maybe we should force users to import
    // algorithm and stdio?
    d   ~= "import std.algorithm;\n";
    d   ~= "import std.stdio;\n";
    d   ~= "import std.string;\n";

    // Interface
    cpp ~= "@qdmd.cpp@ class " ~ T ~ " {\n";
    cpp ~= "@qdmd.cpp@ public:\n";
    foreach(id; identifiers) {
        if (id.protection == "private") {
            continue;
        }
        cpp ~= "@qdmd.cpp@ \t" ~ id.cInterface() ~ "\n";
    }
    cpp ~= "@qdmd.cpp@ };\n";
    cpp ~= "@qdmd.cpp@ \n";

    // CTR / DTR / CONVS
    d   ~= T ~ " _refs_" ~ T ~ "[]; \n";
    cpp ~= "@qdmd.cpp@ " ~ T ~ "* (*_new_" ~ T ~ ")() = 0;\n";
    d   ~= "extern(C++) " ~ T ~ " _new_" ~ T ~ "() { auto r = new " ~ T ~ "; _refs_" ~ T ~ " ~= r; return r; }\n";

    cpp ~= "@qdmd.cpp@ void (*_del_" ~ T ~ ")(" ~ T ~ "*) = 0;\n";
    d   ~= "extern(C++) void _del_" ~ T ~ "(" ~ T ~ " _arg) { _refs_" ~ T ~ " = _refs_" ~ T ~ ".remove!(a => a != _arg)(); }\n";

    cpp ~= "@qdmd.cpp@ " ~ identifiers.map!(id => id.cConvPtrTypes(T)).join("").replace("\n", "\n@qdmd.cpp@ ") ~ "\n";
    d   ~= identifiers.map!(id => id.dConvPtrImpl(T)).join("").replace("\n", "\n ");

    cpp ~= "@qdmd.cpp@ extern \\\"C\\\" void _reg_" ~ T ~ "(" ~
                T ~ "* (*c_new_" ~ T ~ ")()," ~
                "void (*c_del_" ~ T ~ ")(" ~ T ~ "*)" ~
                identifiers.map!(id => id.cConvPtrDecls(T)).join("").replace("\n", " ") ~
                ") {" ~
                    "_new_" ~ T ~ " = c_new_" ~ T ~ ";" ~
                    "_del_" ~ T ~ " = c_del_" ~ T ~ ";" ~
                    identifiers.map!(id => id.cConvPtrAsns(T)).join("").replace("\n", "\n@qdmd.cpp@") ~
                "}\n";

    d   ~= "extern(C++) alias " ~ T ~ " function() c_new_" ~ T ~ ";\n";
    d   ~= "extern(C++) alias void function(" ~ T ~ ") c_del_" ~ T ~ ";\n";
    d   ~= identifiers.map!(id => id.dConvPtrTypes(T)).join("\n");
    d   ~= "extern(C) void _reg_" ~ T ~ "(c_new_" ~ T ~ ", c_del_" ~ T ~ 
        identifiers.map!(id => id.dConvPtrDecls(T)).join("") ~
        ");\n";
    d   ~= "static this() {\n";
    d   ~= "    _reg_" ~ T ~ "(&_new_" ~ T ~ ", &_del_" ~ T ~ 
        identifiers.map!(id => id.dConvPtrRefs(T)).join("").replace("\n", "\n@qdmd.cpp@") ~ ");\n";
    d   ~= "}\n";

    cpp ~= "@qdmd.cpp@ \n";

    // Shim
    cpp ~= "@qdmd.cpp@ class " ~ T ~ "Shim : public QQuickItem {\n";
    cpp ~= "@qdmd.cpp@ Q_OBJECT \n";
    cpp ~= "@qdmd.cpp@ \t" ~ T ~ "* m_itfce;\n";
    cpp ~= "@qdmd.cpp@ \n";
    cpp ~= "@qdmd.cpp@ public:\n";
    cpp ~= "@qdmd.cpp@ " ~ qtProperties(identifiers).join("\n@qdmd.cpp@ ") ~ "\n";
    cpp ~= "@qdmd.cpp@ \tstatic QMap<" ~ T ~ "*," ~ T ~ "Shim*> s_map;\n";
    cpp ~= "@qdmd.cpp@ \t" ~ T ~ "Shim() : m_itfce(_new_" ~ T ~ "()) {s_map[m_itfce] = this;}\n";
    cpp ~= "@qdmd.cpp@ \tvirtual ~" ~ T ~ "Shim() { _del_" ~ T ~ "(m_itfce); }\n";
    cpp ~= "@qdmd.cpp@ \n";
    foreach(id; identifiers) {
        if (id.protection == "private") {
            continue;
        }
        cpp ~= "@qdmd.cpp@ \t" ~ id.shim(T) ~ "\n";
    }
    cpp ~= "@qdmd.cpp@ };\n";
    cpp ~= "@qdmd.cpp@ \tQMap<" ~ T ~ "*," ~ T ~ "Shim*> " ~ T ~ "Shim::s_map;\n";
    foreach(id; identifiers) {
        if (!id.isSignal) {
            continue;
        }
        cpp ~= "@qdmd.cpp@ " ~ id.cImpl(module_, T) ~ "\n";
        d ~= id.dImpl(T);
    }
    cpp ~= "@qdmd.cpp.main@ qmlRegisterType<" ~ T ~ "Shim>(\\\"" ~
        module_ ~ "\\\"" ~
        ", " ~ major.to!string ~ ", " ~ minor.to!string ~ ", \\\"" ~ T ~ "\\\");";

    // Madness:
    return "pragma(msg, \"" ~ cpp ~ "\");" ~ d;
}

enum qtJsonify = "jsonify";
enum qtSignal = "qtSignal";

extern(C) void qdmd_main(const char* qml);
