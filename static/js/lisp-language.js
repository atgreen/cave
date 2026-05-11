// Monaco Monarch tokenizer for Common Lisp
(function() {
  if (typeof monaco === 'undefined') return;
  if (monaco.languages.getLanguages().some(function(l){return l.id==='lisp'})) return;

  monaco.languages.register({ id: 'lisp', extensions: ['.lisp','.cl','.asd','.lsp'] });
  monaco.languages.setMonarchTokensProvider('lisp', {
    defaultToken: '',
    brackets: [['(',')',''],['{','}','']],
    keywords: [
      'defun','defmacro','defvar','defparameter','defconstant','defclass',
      'defmethod','defgeneric','defpackage','defstruct','deftype','defsetf',
      'define-condition','define-compiler-macro','define-modify-macro',
      'define-setf-expander','define-symbol-macro','define-method-combination',
      'lambda','let','let*','flet','labels','macrolet','symbol-macrolet',
      'block','return-from','tagbody','go','catch','throw','unwind-protect',
      'progn','prog1','prog2','multiple-value-bind','multiple-value-prog1',
      'if','cond','when','unless','case','ecase','typecase','etypecase',
      'and','or','not','setf','setq','push','pop','incf','decf',
      'do','do*','dolist','dotimes','loop','return',
      'in-package','use-package','export','import','require',
      'format','funcall','apply','values','the','declare','ignore','type',
      'handler-case','handler-bind','restart-case','with-slots','with-accessors',
      'with-open-file','with-output-to-string','destructuring-bind',
      'make-instance','slot-value','call-next-method',
      'eq','eql','equal','equalp','null','nil','t'
    ],
    tokenizer: {
      root: [
        [/#\\./, 'string'],
        [/;.*$/, 'comment'],
        [/#\|/, 'comment', '@blockComment'],
        [/"/, 'string', '@string'],
        [/#'/, 'keyword'],
        [/'/, 'keyword'],
        [/`/, 'keyword'],
        [/,@?/, 'keyword'],
        [/[(]/, 'delimiter'],
        [/[)]/, 'delimiter'],
        [/:[\w\-+*!?<>=\/.]+/, 'constant'],
        [/&[\w\-]+/, 'type'],
        [/#[bBoOxX][0-9a-fA-F]+/, 'number'],
        [/-?[0-9]+\/[0-9]+/, 'number'],
        [/-?[0-9]*\.[0-9]+([eEdD][+-]?[0-9]+)?/, 'number'],
        [/-?[0-9]+/, 'number'],
        [/[\w\-+*!?<>=\/.]+/, { cases: { '@keywords': 'keyword', '@default': 'identifier' } }],
      ],
      string: [
        [/[^"\\]+/, 'string'],
        [/\\./, 'string.escape'],
        [/"/, 'string', '@pop'],
      ],
      blockComment: [
        [/[^|#]+/, 'comment'],
        [/\|#/, 'comment', '@pop'],
        [/[|#]/, 'comment'],
      ],
    }
  });
})();
