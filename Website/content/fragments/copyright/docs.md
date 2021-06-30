---
fragment: content
sidebar:
  sticky: true
title: Documentation
weight: 300
---

### Note

Use one instance of this fragment per page. Running more might lead to unexpected issues.

### Menus

- .Site.Menus.copyright_footer

### Variables

#### copyright
*type: string*  
*default: Copyright {{ $Year }} {{ .Site.params.name }} (i18n enabled)*

#### attribution
*type: boolean*  
*default: false*

If set to true, the Syna theme name and link would be shown using the `attribution` snippet from `i18n`.

[Global variables]({{< ref "global-variables" >}}) are documented as well and have been omitted from this page.
