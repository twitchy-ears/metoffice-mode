# metoffice-mode
A simple wrapper mode to metoffice.el to show the current and upcoming weather in the modeline in a compact way

Get [metoffice.el](https://github.com/hillwithsmallfields/JCGS-emacs/blob/master/information-management/metoffice.el) working first.

Then you should probably use the ever useful use-package

```
(use-package metoffice
  :init (setq metoffice-home-location "xxxxxx"
              metoffice-api-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
  :config (metoffice-setup))

(use-package metoffice-mode
  :requires metoffice
  :config (metoffice-mode))
```

And made sure that ~/.metoffice-config.el contains

```
(setq metoffice-api-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
```

Alternatively do it by hand, not tested but something like this?

```
(setq metoffice-home-location "xxxxxx"
     metoffice-api-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
(metoffice-setup)
(require 'metoffice)
(require 'metoffice-mode)
(metoffice-mode)
```

Honestly I'd just try use-package as that's actually tested properly.
