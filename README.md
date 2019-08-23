# downdraft
A writing timer for Emacs in the style of [The Most Dangerous Writing Prompt Generator](https://www.squibler.io/writing-prompt-generator). If you don't keep adding text to the end of the buffer within a set time limit (default 5 seconds), the buffer will be deleted.

Also, it is set up so the remaining time will not be refilled if previous parts of the text are deleted until the maximum amount of characters that have been typed so far is surpassed again. This is to force you to write new text for first drafts instead of stalling or spending time editing.

## Usage
Add these lines to your init script.

```elisp
(require 'downdraft)
(downdraft-add-to-mode-line)
```

Then, call either `downdraft-time` or `downdraft-word-count` to
start a new draft.
