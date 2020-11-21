XEdit Text Editor for Color Maximite 2 by Epsilon
-------------------------------------------------
Current Version: 0.3

ChangeLog
---------
0.3:
- Limited MMBasic Syntax Highlighting support.
- Improved vertical scrolling, line insertion and line deletion speed.
- Switched to a dark solarized color scheme. If you prefer the higher contrast blue&white theme,
  you can still select that in the user config variables section. Or you can just roll your own
  of course.
- Critical bug fix: potential program abort triggered by toggling window split. This bug was 
  introduced in 0.2.
- Critical bug fix: wrap-around search without hit can leave editor in inconsistent state,
  resulting in unpredictable behavior.
- Fixed bug in replace function turning parts of replaced lines into upper case. This bug was
  introduced in 0.2.
- Macro recording fix: new macro should overwrite existing macro, not append to it. This bug was
  introduced in 0.2.
- Added DISABLE_CONFIRMATION prompt option. Default: 0.
- Fixed a bug where last lines of a buffer would show up duplicate after deleting a selection.
- Made sure that search matches are always shown fully on screen.
- Increased undo depth from 16 to 32 entries.
- Split view into a single buffer will now as much as possible maintain view in one window
  (instead of scrolling) when lines are added or removed in other window.
- Now allowing replace-with-nothing.

0.2:
- Cleaned up and fixed typo in help screen.
- Fixed AltGr keycombos (such as @) on Azerty keyboards
- Fixed buffer position not being remembered after full screen toggle buffer followed by toggle
  window split.
- Fixed incorrect indentation of newline when cursor was in the leading spaces section.
- Fixed undo enter not working correctly.
- Fixed crash(!) that could happen when going from single window to split screen mode and 
  then toggling active window.
- Fixed match on last window row not being shown after search wraparound.
- Grouped user config settings at beginning of xedit.bas.
- Added user config. setting: SEARCH_IS_CASE_SENSITIVE%. Default=0. 
- Added configurable option to restore previous context when starting editor. Default enabled.
- Added Alt-F find next function.
- Included string to find/replace in search prompt.
- Added macro recording capability, bound to F7/F8 (start/stop&playback).
- Added Close File action (F12).

0.1:
- Initial version.

Description
-----------
XEdit is a text editor written in MMBasic. The editor supports up to two windows (Hsplit/Vsplit
or no split) and up to two buffers (files). The two buffers can be freely assigned to 
windows and two windows can present separate views into a single buffer.
XEdit supports undo, macro recording, MMBasic syntax highlighting as well as the usual 
complement of editor operations: cut/copy/paste, find/replace, indent/unident selections, 
insert/overwrite mode, goto line.

To Do's
-------
- Add support for vegipete's FileDialog.
- Add option to display row numbers next to the rows.
- Add visual indicators of position within buffer.
- Add Select-All key binding.
- Maintain backup files when saving.
- Add scroll current line to center/top/end key bindings.
- Add resource utilization pop-up.
- Add kill-to-end-of-line (Ctrl-K) keybinding.
- Add case senstive search/replace keybindings.
- Improve horizontal scrolling speed.
- Improve search speed.

Key Bindings
------------
F1         = Help
F2/F9      = Save File/Save File as
F3         = Load File
F4         = Toggle Buffer
F5         = Toggle Window split
F10        = Quit
Ctrl-O     = Toggle Active Window
Ctrl-F     = Find Prompt/Selection
Alt-F      = Find Next
Ctrl-R     = Replace Prompt/Selection
Ctrl-X/Y/P = Cut/Copy/Paste
Ctrl-G     = Goto Line
INS        = Toggle Insert/Overwrite mode
Home 1x/2x/3x = Go To Start of Line/Page/Buffer
End 1x/2x/3x = Go To End of Line/Page/Buffer
Tab/Shift-Tab = Indent/Unindent Line/Selection
Shift-Navigation Key = Start/Extend Selection
Ctrl-Z     = Undo
F7         = Start Macro Recording
F8         = Stop Macro Recording / Playback recorded macro
Alt-C      = Toggle Syntax Highlighting On/Off
Alt-K      = Show Key Code at prompt
Alt-S      = Screenshot

Limitations
-----------
- Max. 14000 lines across all buffers (including clipboard and undo buffer).
- Max. 2 windows
- Max. 2 buffers
- Console Only
- Text entered on find prompt can't include double quotes or commas. Find selection can
includes double quotes and commas however.
- Tested on FW version 5.0505 only.
                                                                                                                               