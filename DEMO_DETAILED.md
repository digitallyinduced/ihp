# HSX Enhanced Error Messages - Before & After Demo

## Issue #916 - Enhanced HSX Parser Error Messages

### Example 1: Invalid Tag Name

**BEFORE (Basic Error):**
```
Parse error: unexpected 'spn'
at line 1, column 5
```

**AFTER (Enhanced Error):**
```
Invalid tag name: spn
'spn' is not a valid HTML tag name
Check if you meant a similar valid tag like: div, span, or check the HTML specification
For custom web components, use kebab-case with at least one hyphen (e.g., 'my-component')
Use 'uncheckedHsx' if you need to use non-standard tag names
```

### Example 2: Invalid Attribute Name

**BEFORE (Basic Error):**
```
Parse error: unexpected 'clas'
```

**AFTER (Enhanced Error):**
```
Invalid attribute name: clas
'clas' is not a valid HTML attribute name
Valid HTML attributes include standard ones like 'class', 'id', 'style'
Data attributes must start with 'data-' (e.g., 'data-toggle')
ARIA attributes must start with 'aria-' (e.g., 'aria-label')
Use 'customHsx' with additionalAttributeNames if you need custom attributes
```

### Example 3: Duplicate Attribute

**BEFORE (Basic Error):**
```
Parse error: duplicate attribute
```

**AFTER (Enhanced Error):**
```
Duplicate attribute: class
Attribute 'class' appears multiple times in the same tag
Remove duplicate attributes
If you need conditional attributes, use Haskell expressions
```

### Benefits

✅ **Clear identification** of the exact problem
✅ **Contextual suggestions** based on error type  
✅ **Educational guidance** about HTML standards
✅ **Alternative solutions** provided
✅ **Better developer experience** for HSX templates

---
*This implementation significantly improves HSX debugging and reduces developer frustration.*
