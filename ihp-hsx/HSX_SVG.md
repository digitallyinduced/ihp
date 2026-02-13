# SVG Support in HSX

HSX supports inline SVG out of the box via the standard `[hsx||]` quasiquoter. No separate quasiquoter or special configuration is needed.

## Basic Usage

```haskell
[hsx|
    <svg viewBox="0 0 200 200">
        <circle cx="100" cy="100" r="80" fill="blue"/>
        <rect x="10" y="10" width="50" height="50" fill="red" opacity="0.5"/>
        <path d="M10,10 L190,190" stroke="black" stroke-width="2"/>
        <text x="100" y="105" text-anchor="middle" fill="white">Hello</text>
    </svg>
|]
```

## Dynamic Attributes

Haskell expressions work in SVG attributes just like in HTML:

```haskell
let color = "red" :: Text
    radius = "40" :: Text
[hsx|<circle cx="50" cy="50" r={radius} fill={color}/>|]
```

## Spliced Content

Haskell expressions can be spliced into SVG text content:

```haskell
let label = "Hello SVG" :: Text
[hsx|<text x="10" y="30">{label}</text>|]
```

## Spread Attributes

The `{...}` spread syntax works on SVG elements:

```haskell
let svgAttrs :: [(Text, Text)] = [("fill", "red"), ("stroke", "black")]
[hsx|<rect width="100" height="100" {...svgAttrs}/>|]
```

## Supported SVG Elements

All standard SVG elements from the [MDN SVG Element Reference](https://developer.mozilla.org/en-US/docs/Web/SVG/Element) are supported as parent (non-void) elements:

### Container Elements
`svg`, `g`, `defs`, `symbol`, `use`, `image`, `foreignObject`, `switch`, `view`

### Shape Elements
`circle`, `ellipse`, `line`, `path`, `polygon`, `polyline`, `rect`

### Text Elements
`text`, `tspan`, `textPath`, `desc`, `metadata`, `title`

### Gradient & Pattern Elements
`linearGradient`, `radialGradient`, `stop`, `pattern`

### Clip/Mask Elements
`clipPath`, `mask`, `marker`

### Filter Elements
`filter`, `feBlend`, `feColorMatrix`, `feComponentTransfer`, `feComposite`, `feConvolveMatrix`, `feDiffuseLighting`, `feDisplacementMap`, `feDistantLight`, `feDropShadow`, `feFlood`, `feFuncA`, `feFuncB`, `feFuncG`, `feFuncR`, `feGaussianBlur`, `feImage`, `feMerge`, `feMergeNode`, `feMorphology`, `feOffset`, `fePointLight`, `feSpecularLighting`, `feSpotLight`, `feTile`, `feTurbulence`

### Animation Elements
`animate`, `animateMotion`, `animateTransform`, `set`, `mpath`

### Experimental/Draft Elements
`discard`, `hatch`, `hatchpath`, `mesh`, `meshgradient`, `meshpatch`, `meshrow`

## Supported SVG Attributes

All attributes from the [MDN SVG Attribute Reference](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute) are supported. Key categories include:

### Presentation Attributes
`fill`, `fill-opacity`, `fill-rule`, `stroke`, `stroke-width`, `stroke-dasharray`, `stroke-dashoffset`, `stroke-linecap`, `stroke-linejoin`, `stroke-miterlimit`, `stroke-opacity`, `opacity`, `color`, `display`, `visibility`, `overflow`, `cursor`, `pointer-events`, `paint-order`, `vector-effect`, `shape-rendering`, `image-rendering`, `text-rendering`

### Text Attributes
`font-family`, `font-size`, `font-size-adjust`, `font-stretch`, `font-style`, `font-variant`, `font-weight`, `text-anchor`, `text-decoration`, `text-overflow`, `dominant-baseline`, `alignment-baseline`, `baseline-shift`, `letter-spacing`, `word-spacing`, `white-space`, `writing-mode`, `direction`, `unicode-bidi`

### Geometry & Transform Attributes
`x`, `y`, `x1`, `y1`, `x2`, `y2`, `cx`, `cy`, `r`, `rx`, `ry`, `d`, `width`, `height`, `viewBox`, `transform`, `transform-origin`, `preserveAspectRatio`, `points`, `pathLength`

### Gradient Attributes
`gradientTransform`, `gradientUnits`, `spreadMethod`, `fx`, `fy`, `fr`, `offset`, `stop-color`, `stop-opacity`

### Filter Attributes
`filterUnits`, `primitiveUnits`, `stdDeviation`, `baseFrequency`, `numOctaves`, `in`, `in2`, `mode`, `operator`, `result`, `kernelMatrix`, `kernelUnitLength`, `edgeMode`, `diffuseConstant`, `specularConstant`, `specularExponent`, `surfaceScale`, `elevation`, `azimuth`, `limitingConeAngle`, `pointsAtX`, `pointsAtY`, `pointsAtZ`, `scale`, `seed`, `stitchTiles`, `bias`, `divisor`, `order`, `preserveAlpha`, `radius`, `targetX`, `targetY`, `xChannelSelector`, `yChannelSelector`, `flood-color`, `flood-opacity`, `lighting-color`, `color-interpolation-filters`

### Marker Attributes
`markerHeight`, `markerWidth`, `markerUnits`, `refX`, `refY`, `orient`

### Mask/Clip Attributes
`clipPathUnits`, `maskContentUnits`, `maskUnits`, `mask-type`, `clip-path`, `clip-rule`

### Animation Attributes
`attributeName`, `begin`, `dur`, `end`, `from`, `to`, `by`, `values`, `keyTimes`, `keySplines`, `keyPoints`, `calcMode`, `repeatCount`, `repeatDur`, `restart`, `accumulate`, `additive`

### Link Attributes (deprecated but supported)
`xlink:href`, `xlink:arcrole`, `xlink:show`, `xlink:title`, `xlink:type`, `xlink:actuate`, `xlink:role`

### Other
`id`, `class`, `style`, `lang`, `tabindex`, `decoding`, `side`, `href`, `target`, `systemLanguage`, `requiredExtensions`

Additionally, `data-*`, `aria-*`, and `hx-*` prefixed attributes are supported via prefix matching.

## Using customHsx for Additional SVG Elements

If you need SVG elements or attributes that aren't in the built-in whitelist, use `customHsx` to extend:

```haskell
import IHP.HSX.QQ (customHsx)
import IHP.HSX.Parser (HsxSettings(..))
import qualified Data.Set as Set
import Language.Haskell.TH.Quote (QuasiQuoter)

mySvgHsx :: QuasiQuoter
mySvgHsx = customHsx
    HsxSettings
        { checkMarkup = True
        , additionalTagNames = Set.fromList ["myCustomSvgElement"]
        , additionalAttributeNames = Set.fromList ["my-custom-attr"]
        }
```

Then use it:

```haskell
[mySvgHsx|<svg><myCustomSvgElement my-custom-attr="value"/></svg>|]
```

Alternatively, use `uncheckedHsx` to skip all element/attribute validation entirely:

```haskell
import IHP.HSX.QQ (uncheckedHsx)

[uncheckedHsx|<svg><anyElement anyAttribute="value"/></svg>|]
```

## Rendering Notes

- SVG elements are rendered as regular HTML parent elements (with opening and closing tags). Self-closing syntax `<circle/>` in HSX produces `<circle></circle>` in the output, which is valid for SVG embedded in HTML5.
- Static SVG subtrees are pre-rendered to byte strings at compile time for optimal performance.
- The parser validates element and attribute names at compile time, catching typos with edit-distance suggestions.
- Both the Blaze and Lucid2 backends support SVG identically.

## Audit Summary

The SVG element and attribute whitelists were audited against the [MDN SVG Element Reference](https://developer.mozilla.org/en-US/docs/Web/SVG/Element) and [MDN SVG Attribute Reference](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute).

### Elements Added
| Element | Purpose |
|---------|---------|
| `image` | Embeds raster images in SVG (distinct from HTML `<img>`) |

### Attributes Added
| Attribute | Purpose |
|-----------|---------|
| `decoding` | Image decoding hint for `<image>` |
| `fr` | Focal radius for `<radialGradient>` |
| `mask-type` | Luminance vs alpha masking for `<mask>` |
| `paint-order` | Controls fill/stroke/markers paint order |
| `side` | Which side of the path text is placed on (experimental) |
| `text-overflow` | Text overflow handling presentation attribute |
| `transform-origin` | Origin point for transformations |
| `vector-effect` | Vector effect (e.g. `non-scaling-stroke`) |
| `white-space` | Whitespace handling presentation attribute |

### Attributes Intentionally Not Added
| Attribute | Reason |
|-----------|--------|
| `fetchpriority` | Both experimental and non-standard (double-flagged on MDN) |

### Pre-existing Coverage
All other SVG elements and attributes from the MDN reference were already present in the parser whitelist prior to this audit.
