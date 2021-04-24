XEdit Text Editor for Color Maximite 2 by Epsilon
-------------------------------------------------
Current Version: 0.10

ChangeLog
---------
0.10
- Find-across-files integration fixes. Find-across-files in 0.9 can lead to program crashes.
- Find-across-files with empty filespec now correctly search current working dir + subdirs.
- Fixed program abort that happened when providing empty name is Save File Dialogue box.
- Added Current Working Directory to footer.
- Support selection based xFind.
- Checks and aborts loading of file if file has too-long lines.
- Added more keywords for syntax highlighting.
- Added check if user added quotes around filename on command line. Removed them if he did.
- Split xedit.bas into multiple files. Each file is now editable again using the built-in editor,
  a good citizenship requirement :-)
- Added support for user config and keybindings that can persist across version updates. Copy
keybinding.default.inc to keybinding.user.inc, then make your tweaks in keybinding.user.inc as
you see fit, and you're good to go. Same with settings.user.inc.
- Included a self-extracting archive: xedit.mar.bas so you don't have to transfer all files in
the directory separately to your CMM2. Just transfer xedit.mar.bas and execute it to have it
unpack itself into a xedit/ directory.

0.9
- Integrated xFind find-across-files functionality, bound to Ctrl-S.
- Bug fix: Selection not cleared on copy action.
- Bug fix: Horizontal scrolling with active selection not displayed correctly.
- Changed some defaults: NUM_BACKUP_FILES% defaults to 0. RESTORE_PREV_SESSION_CTXT% defaults to 0.
- Buf fix: Fixed crash when trying to join two lines with a combined length > 255.
- Added more syntax highlighting keywords.

0.8
- Some color tweaks
- Added support for vegipete's FileDialog to load and save files.

0.7:
- Removed limitation: Text entered on find prompt can now include quotes and commas.
- Added more keywords for syntax highlighting.
- Fixed error that sometimes occurs when exiting, preventing context from being saved.
- Position-in-buffer indicator now updates when searching.
- Disable syntax highlighting if buffer is not MMBasic.
- Improved cursor position restore when toggling between buffers (F4).
- Accept two files to open on command line.

0.6:
- Critical Bug Fix: Pressing enter twice on last line in window triggered an incorrect page
  redraw action (scroll down instead of up) so it would look as if the line disappeared.
- Critical Bug Fix: Ctrl-X (Cut) without active selection triggers program abort.
- Bug Fix: In SERIAL_INPUT_COMPAT_MODE, toggle selection (Esc) was broken in 0.5.
- In SERIAL_INPUT_COMPAT_MODE replaced Ctrl-B with Shift-Tab. This key binding only works
  with V5.06 FW.
- Added more keywords for syntax highlighting.
- New key binding: Ctrl-M = Scroll current line to Center of Window
- New key binding: Alt-M  = Scroll current line to Top of Window
- V5.06 FW compatilibity fixes.
  
0.5:
- Improved horizontal scrolling speed.
- More sensible buffer position when toggling window split (F5).
- Faster vertical scrolling with active selection (shift-crsrUp/Down).
- Ctrl-K deletes from cursor to End Of Line.
- Fixed some laggy selection highlighting cases.
- Added User Configurable variable NUM_BACKUP_FILES specifying how many backup copies to
  maintain when saving a file. Defaults to 1.
- Added editor resource utilization pop-up, bound to Alt-R.
- Add Select-All key binding Ctrl-A.
- Avoiding unnecessary horizontal scrolling when moving cursor up or down.  
- Reorganized help page.
- Fixed corner case where "Paste done." message doesn't appear after completing a past action.
- Fixed corner case: when undo buffer was full and needed to be cleared (emergency measure),
  the undo buffer lines weren't returned to the line pool, creating a memory leak.

0.4:
- Added visual position-in-buffer indicator on right window border.
- Improved search speed.
- Fixed out of memory error when opening help screen in certain configurations, 
  reported by thwill.
- Fixed weird 'animation' on last line when (un)indenting a selection that spans more than
  1 screen. Also made it much faster.
- Dynamically sized keyword array (KEYWORD_LIST_DATA) using end sentinel as suggested by Jiri.
- Fixed bug: Search always started from the beginning of the line instead of from cursor.
- Added serial compatibility mode, enabled through flag SERIAL_INPUT_COMPAT_MODE. Note that
  enabling this mode affects certain keybindings. In particular, selection mode is toggled 
  using the Esc key.
- Moved find-next keybinding to Ctrl-N. Added find-previous function bound to Alt-N.
- Added reverse-find keybinding, Alt-F.
- F6 shows console screen as it was when launching xedit in current buffer. This allows you to
  see your previous program output, trace list etc. from xedit.
- F11 exits XEdit and runs program currently in buffer.

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

To Dos
------
- Add case sensitive search/replace keybindings.
- Support copy-to-clipboard from console screen.
- Saw a case once where vertical scrolling led to the bottom row being displayed twice. Need to reproduce and investigate.
- SaveFileDialog can't be aborted when entering filename already started.
- BackSpace in SaveFileDialog can go outside of box. Pressing enter at that point leads to
  program abort.
- Smarter indentation handling.

Key Bindings
------------
(Ref. Key Bindings section in XEdit.bas to modify)

F1          = Help
F2/F9       = Save File/Save File as
F3          = Load File
F12         = Close File
F4          = Toggle Buffer
F5          = Toggle Window split
F6          = Show Console Screen in current buffer
F10         = Exit XEdit
F11         = Exit XEdit and run program currently in buffer
Ctrl-O      = Toggle Active Window
Ctrl/Alt-F  = Forward/Reverse Find Prompt or Selection
Ctrl/Alt-N  = Find Next/Previous
Ctrl-R      = Replace Prompt or Selection
Ctrl-X/Y/V  = Cut/Copy/Paste
Ctrl-K      = Delete from cursor to End Of Line
Ctrl-G      = Goto Line
INS         = Toggle Insert/Overwrite mode (Ctrl-W in SERIAL_INPUT_COMPAT_MODE)
Home 1/2/3x = Go To Start of Line/Page/Buffer
End 1/2/3x  = Go To End of Line/Page/Buffer
Ctrl-M      = Scroll current line to Center of Window
Alt-M       = Scroll current line to Top of Window
Tab/Shift-Tab = Indent/Unindent Line/Selection
Shift-Navigation Key = Start/Extend Selection
(Esc toggles selection mode in SERIAL_INPUT_COMPAT_MODE)
Ctrl-A      = Select All
Ctrl-Z      = Undo
F7          = Start Macro Recording
F8          = Stop Macro Recording / Playback recorded macro
Alt-C       = Toggle Syntax Highlighting On/Off (Ctrl-S in SERIAL_INPUT_COMPAT_MODE)
Alt-K       = Show Key Code at prompt
Alt-R       = Show XEdit Resource Utilization
Alt-S       = Screenshot

Navigation Keys: Cursor Up/Down/Left/Right, PgUp/Down, Home/End.

User Configurable Settings 
--------------------------
(Set at start of XEdit.bas)

SEARCH_IS_CASE_SENSITIVE%=0/1      Default=0
TAB_WIDTH%=<Num.>                  Default=2
RESTORE_PREV_SESSION_CTXT%=0/1     Default=1
FG/KEYWORD/STRING/COMMENT/BG_COLOR%
DISABLE_CONFIRMATION_PROMPTS%=0/1  Default=0
NUM_BACKUP_FILES%=<Num.>           Default=1
SERIAL_INPUT_COMPAT_MODE%=0/1      Default=0
(Alt-based key bindings N/A in SERIAL_INPUT_COMPAT_MODE)

Limitations
-----------
- Max. 14000 lines across all buffers (including clipboard and undo buffer).
- Max. 2 windows
- Max. 2 buffers
- Edit and Clipboard operations are not supported on buffer in show console mode.
- No undo for replace operations.

