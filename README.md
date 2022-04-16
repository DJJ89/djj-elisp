# Boomrang a quick way to yank regexp from the buffer without moving.

```elisp
(require 'avy) ; available on melpa
(require 'boomrang)
(require key-chord ) ; available on melpa
(key-chord-define-global (kbd "km") 'boomrang)
(key-chord-define-global (kbd "jn") 'boomrang2)
```
