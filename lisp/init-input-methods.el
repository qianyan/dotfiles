(require 'pyim)
(require 'pyim-basedict)
(require 'pyim-cregexp-utils)
(require 'pyim-cstring-utils)

;; 如果使用 popup page tooltip, 就需要加载 popup 包。
(require 'popup nil t)
(setq pyim-page-tooltip 'popup)

;; 如果使用 pyim-dregcache dcache 后端，就需要加载 pyim-dregcache 包。
;; (require 'pyim-dregcache)
;; (setq pyim-dcache-backend 'pyim-dregcache)

;; 加载 basedict 拼音词库。
(pyim-basedict-enable)

;; 将 Emacs 默认输入法设置为 pyim.
(setq default-input-method "pyim")

;; 显示 7 个候选词。
(setq pyim-page-length 7)

;; 金手指设置，可以将光标处的编码（比如：拼音字符串）转换为中文。
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

(pyim-scheme-add
 '(sogou-shuangpin
   :document "搜狗双拼方案。"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz;"
   :prefer-triggers nil
   :cregexp-support-p t
   :keymaps
   (("a" "a" "a")
    ("b" "b" "ou")
    ("c" "c" "iao")
    ("d" "d" "iang" "uang")
    ("e" "e" "e")
    ("f" "f" "en")
    ("g" "g" "eng")
    ("h" "h" "ang")
    ("i" "ch" "i")
    ("j" "j" "an")
    ("k" "k" "ao")
    ("l" "l" "ai")
    ("m" "m" "ian")
    ("n" "n" "in")
    ("o" "o" "uo" "o")
    ("p" "p" "un")
    ("q" "q" "iu")
    ("r" "r" "uan" "er")
    ("s" "s" "iong" "ong")
    ("t" "t" "ue" "ve")
    ("u" "sh" "u")
    ("v" "zh" "ui")
    ("w" "w" "ia" "ua")
    ("x" "x" "ie")
    ("y" "y" "uai" "v")
    ("z" "z" "ei")
    (";" ";" "ing")
    ("oa" "a")
    ("oj" "an")
    ("ol" "ai")
    ("ok" "ao")
    ("oh" "ang")
    ("oe" "e")
    ("oz" "ei")
    ("of" "en")
    ("or" "er")
    ("og" "eng")
    ("oo" "o")
    ("ob" "ou"))))
;; (pyim-default-scheme 'quanpin)
;; (pyim-default-scheme 'kkwubi)
;; (pyim-default-scheme 'cangjie)

(pyim-default-scheme 'sogou-shuangpin)
;; 设置 pyim 是否使用云拼音
;; (setq pyim-cloudim--parse-baidu-buffer 'baidu)

;; 设置 pyim 探针
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))

;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))

;; 开启代码搜索中文功能（比如拼音，五笔码等）
(pyim-isearch-mode 1)
(global-set-key (kbd "M-f") 'pyim-forward-word)
(global-set-key (kbd "M-b") 'pyim-backward-word)

(provide 'init-input-methods)

