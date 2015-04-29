TODO
====

- Accept Header
  - File extension should be a general method to looking something up, accept headers
    should be more rigourus
    - file extension _without_ accept header constraint should lookup _nearest_ available
      content type
      - `.txt` goes to text, json, then html
      - `.json` goes to json, text, then html
      - `.html` goes to html, text, then json
    - accept headers should _remove_ elements from this chain
- `fooStatus` - inject a status code
  - `text`
  - `textStatus`
  - `textHeaders`
  - `textStatusHeaders`
  - `bytestring`
  - `bytestringHeaders`
  - `bytestringStatus`
  - `bytestringStatusHeaders`
- Work with Markup & UrlPath
