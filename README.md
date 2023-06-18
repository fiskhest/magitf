# magitf

*ma-gi-teff*

Drawing inspiration from the greatest workflow `magit`, `magitf` is a terraform porcelain for emacs!

Admittedly, experiencing terraform interactively in a terminal can be many things but joyful, with the many various options to commands and parsing of changes quickly becoming an intangible mess. `magitf` is a take on integrating terraform into emacs where we supercharge terraform with transients. Running terraform inside of emacs with other workflows should hopefully be a breeze.

## Installation

Should probably be configured to upload new releases to a package repository such as MELPA or some such for a more automated onboarding experience, but for now it'll have to be manual.

### Manual
``` sh
export TOOLS_DIRECTORY="~/.emacs.d/modules/tools/magitf"
mkdir -p $TOOLS_DIRECTORY
curl -o $TOOLS_DIRECTORY/magitf.el https://github.com/fiskhest/magitf/blob/main/magitf.el
```

### MELPA

``` sh
M-x package-install RET magitf RET
```

Add magitf as a package to your emacs client
``` sh
# doom:
# ~/.doom/init.el
:tools
        magitf              ; a terraform porcelain for Emacs
```

If you prefer accessing magitf with `C-c t f`, add a global shortcut
``` sh
# doom:
# ~/.doom/config.el
(global-set-key (kbd "C-c t f") 'magitf-status)
```

Alternatively, `SPC g T`
``` sh
# doom:
# ~/.doom/config.el
(when (modulep! :tools magitf)
 (map! :leader
   (:prefix-map ("g" . "git")
          :desc "magitf status"    "T"   #'magitf-status))
```

Alternatively, `SPC <localleader> t`
``` sh
# doom:
# ~/.doom/config.el
(when (modulep! :tools magitf)
 (map! :map terraform-mode-map
       :localleader
       :desc "terraform" "t" #'magitf-status))
```

## Usage

In it's basic form, magitf is available by
- `M-x magitf-status`

If you configured shortcuts for your client manually, the custom ones are available under:
There are multiple ways of accessing the root transient menu:
- `SPC-g T`(erraform)
- `SPC-<localleader> t`(erraform)
- `C-c t f` 

When landing on the root transient menu, a choice between action, utility or control operation is presented.

Available actions:
- `init`
- `validate`
- `plan`
- `apply`
- `destroy`

Available utilities:
- `console`
- `fmt`
- `state`
- `force-unlock`

Each transient submenu has it's own options available to toggle as necessary

Sample screenshot to showcase the workflow:
![magitf in action](root-transient.png)
![magitf submenu in action](submenu-transient.png)

## Whats next

- Break up the results of `tf plan` (both in `action:(plan|apply|destroy)` so that each resource presented in the plan stdout can be broken down into a collapseable block for smoother overview, much like how magit handles staged and unstaged changes per file.
- Upload package to melpa
- add instructions for installing with melpa
- break away doom-isms
- add support for import
- convert README.md to org-mode
