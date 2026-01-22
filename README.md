# PasTemplates
A Go text/template-compatible templating system for Lazarus/FPC

---

## 📌 Overview

This library provides a **powerful, type-safe, and Go-compatible template engine** for Free Pascal. It supports:

- Full Go `text/template` syntax (including `{{block}}`, `{{range}}`, `{{with}}`, pipelines, etc.)
- Runtime-safe access to objects, arrays, and collections via RTTI
- Built-in functions (`len`, `index`, `eq`, `and`, `or`, `not`, `print`, `call`, etc.)
- User-defined functions
- HTML auto-escaping (`{{html .}}`)
- Template composition and inheritance (`{{define}}`, `{{template}}`, `{{block}}`)
- File-based template loading
- Configurable missing-key behavior (`error` / `zero` / `invalid`)
- Whitespace trimming (`{{-` and `-}}`)
- Comments (`{{/* ... */}}`)

It is **fully standalone**, requires only standard FPC units + LazUtils.

---

## 🧩 Core Concepts

### 1. **Templates**
A template is a named unit of logic and text. You create one with:
```pascal
T := New('main');
```

### 2. **Parsing**
Templates are parsed from strings or files:
```pascal
T.Parse('<h1>Hello {{.Name}}</h1>');
// or
T.ParseFile('templates/page.html');
```

### 3. **Execution**
Execute against any data structure (object, array, primitive):
```pascal
Result := T.ExecuteToString(TValue.From(MyData));
```

### 4. **Composition**
Use `{{define "name"}}...{{end}}` and `{{template "name" .}}` to split logic across templates.

### 5. **Blocks (Layout Inheritance)**
```go
<!-- layout.html -->
<title>{{block "title"}}Default{{end}}</title>
<body>{{block "content"}}Default{{end}}</body>

<!-- page.html -->
{{template "layout" .}}
{{define "title"}}User Page{{end}}
{{define "content"}}<h1>Hello {{.Name}}</h1>{{end}}
```

---

## 🔧 API Reference

### **Creating Templates**
| Function | Description |
|--------|-------------|
| `New(Name: string): TTemplate` | Creates a new template with given name |
| `T.Clone: TTemplate` | Deep-copies template with all sub-templates and functions |

### **Configuration**
| Method | Description |
|--------|-------------|
| `T.Delims(L, R: string): TTemplate` | Sets custom delimiters (default: `{{ }}`) |
| `T.Option(['missingkey=error']): TTemplate` | Controls behavior on missing keys |
| `T.Funcs([TFuncDef('upper', @MyUpperFunc)]): TTemplate` | Registers user functions |

### **Parsing**
| Method | Description |
|--------|-------------|
| `T.Parse(Text: string): TTemplate` | Parses template from string |
| `T.ParseFile(FileName: string): TTemplate` | Loads and parses from file |
| `T.ParseFiles([file1, file2]): TTemplate` | Loads multiple templates (e.g., layout + pages) |

### **Execution**
| Method | Description |
|--------|-------------|
| `T.ExecuteToString(Data: TValue): string` | Renders to string |
| `T.Execute(Output: TStrings; Data: TValue)` | Renders to `TStrings` |
| `T.ExecuteStream(Writer: TStream; Data: TValue)` | Streams output  |

### **HTML Safety**
| Feature | Usage |
|--------|-------|
| Auto-escaping helper | `T.HTMLEscape('<script>') → &lt;script&gt;` |
| Built-in `html` function | `{{html .UserInput}}` in templates |

---

## 🛠️ Built-in Functions

| Function | Example | Description |
|--------|--------|-------------|
| `and` | `{{and .A .B}}` | Short-circuit logical AND |
| `or` | `{{or .A .B}}` | Short-circuit logical OR |
| `not` | `{{not .Flag}}` | Logical NOT |
| `len` | `{{len .Items}}` | Length of string/array/TStrings |
| `index` | `{{index .List 0}}` | Access array/string/map element |
| `eq/ne/lt/le/gt/ge` | `{{eq .Status 200}}` | Comparisons |
| `print/println` | `{{print .Name}}` | Convert to string |
| `call` | `{{call "myfunc" .Arg}}` | Call user-defined function by name |

---

## ⚙️ Missing Key Behavior

Control what happens when a field doesn’t exist:

| Mode | Effect |
|------|--------|
| `missingkey=invalid` (default) | Treats missing key as empty value |
| `missingkey=zero` | Same as `invalid` |
| `missingkey=error` | Throws exception on missing key |

Set via:
```pascal
T.Option(['missingkey=error']);
```

---

## 🧪 Supported Data Types

The engine safely accesses:

- **Objects**: published properties (via RTTI)
- **Arrays**: dynamic/static arrays, `TStringDynArray`
- **Collections**: `TStrings`, `TFPList`, `TFPHashObjectList`
- **Primitives**: integers, floats, booleans, strings

All access is **nil-safe** and respects `missingkey` policy.

---

## 📁 File Structure Example

```
templates/
├── layout.html
├── home.html
└── user.html
```

```pascal
T := New('layout').
  ParseFile('templates/layout.html').
  ParseFile('templates/home.html').
  ParseFile('templates/user.html');

// Render home page
HomeOutput := T.Lookup('home').ExecuteToString(TValue.From(HomeData));
```

---


## ✅ Test Coverage

All core features are validated by **41 passing unit tests**, including:

- Template definition and calling
- `{{block}}` inheritance
- Control flow (`if`, `with`, `range`)
- Built-in functions
- HTML escaping
- File loading
- Error handling
- RTTI-based data access
- Whitespace trimming
- Missing-key policies

---

## 📜 License

BSD 3-Clause — free for commercial and open-source use.

---

## 💡 Why Use This?

- **Go compatibility**: Reuse Go template knowledge and patterns.
- **Type safety**: Compile-time checks where possible, safe runtime fallbacks.
- **No external deps**: Pure Object Pascal.

Perfect for **SSR (server-side rendering)**, **email templates**, **config generation**, and **API documentation** in Pascal-based apps.
