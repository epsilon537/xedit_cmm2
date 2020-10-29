XEdit Text Editor for Color Maximite 2 by Epsilon
-------------------------------------------------
Current Version: 0.1

ChangeLog
---------
0.1:
- Initial version.

Description
-----------
XEdit is a text editor written in MMBasic. The editor supports up to two windows (Hsplit/Vsplit
or no split) and up to two buffers (files). The two buffers can be freely assigned to 
windows and two windows can present separate views into a single buffer.
XEdit supports undo as well as the usual complement of operations: cut/copy/paste, find/replace,
indent/unident selections, insert/overwrite mode, goto line.

To Do's
-------
- Add support for vegipete's FileDialog
- Add option to display row numbers next to the rows
- Add Select-All key binding
- Add Macro recording and playback
- Maintain backup files when saving
- Syntax Highlighting
- Add scroll current line to center/top/end key bindings
- Restore previous context when starting editor
- Add Find Next keybinding
- Add resource utilization pop-up
- Add optional config file with user configurable keybindings and other options

Limitations
-----------
- Max. 14000 lines across all buffers (including clipboard and undo buffer).
- Max. 2 windows
- Max. 2 buffers

KeyBindings
-----------
F1 = Help
F2/F9 = Save File/Save File as
F3 = Load File
F4 = Toggle Buffer
F5 = Toggle Window split
F10 = Quit
Ctrl-O = Toggle Active Window
Ctrl-F = Find Prompt/Selection
Ctrl-R = Replace Prompt/Selection
Ctrl-X/Ctrl-Y/Ctrl-P = Cut/Copy/Paste
Ctrl-G = Goto Line
INS = Toggle Insert/Overwrite mode
Home 1x/2x/3x = Go To Start of Line/Page/Buffer
End 1x/2x/3x = Go To End of Line/Page/Buffer
Tab/Shift-Tab = Indent/Unindent Line/Selection
Shift-Navigation Key = Start/Extend Selection
Ctrl-Z = Undo
Alt-K = Show Key Code at prompt
Alt-S = Screenshot
