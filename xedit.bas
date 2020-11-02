OPTION EXPLICIT
OPTION DEFAULT NONE
OPTION BASE 0
OPTION CONSOLE SCREEN

'-->User Configurable Settings:
'-----------------------------
CONST FG_COLOR% = RGB(WHITE)
CONST FG_COLOR2% = RGB(CYAN)
CONST BG_COLOR% = RGB(0,0,128)
CONST BG_COLOR2% = RGB(64, 64, 255)
CONST KEYB_REPEAT_FIRST% = 300
CONST KEYB_REPEAT_REST% = 50

'Set to 1 to make the search function case sensitive
CONST SEARCH_IS_CASE_SENSITIVE% = 0
CONST TAB_WIDTH% = 2
'Set to 1 to try and restore previous context (open files and cursor positions) when open XEdit.
CONST RESTORE_PREV_SESSION_CTXT% = 1
CONST CTXT_FILE_PATH$ = "\.xedit.ctxt"

'<--User Configurable Settings
'-----------------------------

MODE 1, 8
FONT 1, 1

CONST VERSION$ = "0.2"

'--> Key Codes. Press Alt-K in the editor to see the keycode corresponding to a keypress. Combos with Ctrl, Shift and Alt are supported.
'No distinction is made between Left and Right Shift/Alt/Ctrl.
CONST KEY_BCKSPC% = 8
CONST KEY_TAB% = 9
CONST KEY_LF% = 10
CONST KEY_DEL% = 127
CONST KEY_UP_ARROW% = 128
CONST KEY_DOWN_ARROW% = 129
CONST KEY_LEFT_ARROW% = 130
CONST KEY_RIGHT_ARROW% = 131
CONST KEY_INS% = 132
CONST KEY_HOME% = 134
CONST KEY_END% = 135
CONST KEY_PGUP% = 136
CONST KEY_PGDOWN% = 137

CONST KEY_F1% = 145
CONST KEY_F2% = 146
CONST KEY_F3% = 147
CONST KEY_F4% = 148
CONST KEY_F5% = 149
CONST KEY_F6% = 150
CONST KEY_F7% = 151
CONST KEY_F8% = 152
CONST KEY_F9% = 153
CONST KEY_F10% = 154
CONST KEY_F11% = 155
CONST KEY_F12% = 156

CONST KEY_CTRL_F% = 262
CONST KEY_CTRL_G% = 263
CONST KEY_CTRL_Z% = 282
CONST KEY_CTRL_SPC% = 288
CONST KEY_CTRL_O% = 271
CONST KEY_CTRL_R% = 274
CONST KEY_CTRL_V% = 278
CONST KEY_CTRL_X% = 280
CONST KEY_CTRL_Y% = 281

CONST KEY_SHFT_CRSR_U% = 1152
CONST KEY_SHFT_HOME% = 1158
CONST KEY_SHFT_END% = 1159
CONST KEY_SHFT_PGUP% = 1160
CONST KEY_SHFT_PGDOWN% = 1161
CONST KEY_SHFT_CRSR_D% = 1185
CONST KEY_SHFT_CRSR_L% = 1154
CONST KEY_SHFT_CRSR_R% = 1187

CONST KEY_ALT_F% = 614
CONST KEY_ALT_K% = 619
CONST KEY_ALT_L% = 620
CONST KEY_ALT_S% = 627
CONST KEY_SHFT_TAB% = 1033
'<-- Key Codes

'--> Key Bindings:
CONST EXIT_KEY% = KEY_F10%
CONST TOGGLE_SCREEN_SPLIT_KEY% = KEY_F5%
CONST TOGGLE_ACTIVE_WINDOW_KEY% = KEY_CTRL_O%
CONST TOGGLE_INS_OVR_MODE_KEY% = KEY_INS%
CONST TOGGLE_BUFFER_KEY% = KEY_F4%
CONST LOAD_INTO_CURRENT_BUF_KEY% = KEY_F3%
CONST CLOSE_BUFFER_KEY% = KEY_F12%
CONST CRSR_UP_KEY% = KEY_UP_ARROW%
CONST CRSR_DOWN_KEY% = KEY_DOWN_ARROW%
CONST CRSR_LEFT_KEY% = KEY_LEFT_ARROW%
CONST CRSR_RIGHT_KEY% = KEY_RIGHT_ARROW%
CONST HOME_KEY% = KEY_HOME%
CONST END_KEY% = KEY_END%
CONST PGUP_KEY% = KEY_PGUP%
CONST PGDOWN_KEY% = KEY_PGDOWN%
CONST SELECT_CRSR_U_KEY% = KEY_SHFT_CRSR_U%
CONST SELECT_HOME_KEY% = KEY_SHFT_HOME%
CONST SELECT_END_KEY% = KEY_SHFT_END%
CONST SELECT_PGUP_KEY% = KEY_SHFT_PGUP%
CONST SELECT_PGDOWN_KEY% = KEY_SHFT_PGDOWN%
CONST SELECT_CRSR_D_KEY% = KEY_SHFT_CRSR_D%
CONST SELECT_CRSR_L_KEY% = KEY_SHFT_CRSR_L%
CONST SELECT_CRSR_R_KEY% = KEY_SHFT_CRSR_R%
CONST ENTER_KEY% = KEY_LF%
CONST INDENT_KEY% = KEY_TAB%
CONST UNINDENT_KEY% = KEY_SHFT_TAB%
CONST DELETE_KEY% = KEY_DEL%
CONST BACKSPACE_KEY% = KEY_BCKSPC%
CONST TOGGLE_SHOW_KEYCODE_AT_PROMPT% = KEY_ALT_K%
CONST GOTO_KEY% = KEY_CTRL_G%
CONST CUT_KEY% = KEY_CTRL_X%
CONST COPY_KEY% = KEY_CTRL_Y%
CONST PASTE_KEY% = KEY_CTRL_V%
CONST FIND_KEY% = KEY_CTRL_F%
CONST FIND_NEXT_KEY% = KEY_ALT_F%
CONST REPLACE_KEY% = KEY_CTRL_R%
CONST UNDO_KEY% = KEY_CTRL_Z%
CONST SAVE_KEY% = KEY_F2%
CONST SAVE_AS_KEY% = KEY_F9%
CONST HELP_KEY% = KEY_F1%
CONST SCREENSHOT_KEY% = KEY_ALT_S%
CONST START_MACRO_REC_KEY% = KEY_F7%
CONST PLAY_MACRO_KEY% = KEY_F8%


'<-- Key Bindings

CONST MAX_NUM_ROWS% = 14000 'This is the total number of lines available, across all buffers, including clipboard and undo buffer.
CONST NUM_BUFFERS% = 4 'Two regular buffers, the clipboard and the undo buffer.
CONST MAX_NUM_WINDOWS% = 2
CONST MAX_NUM_UNDOS% = 16
CONST MAX_NUM_MACRO_RECORDINGS% = 100
CONST CLIPBOARD_BIDX% = 2 'BIDX = Buffer Index
CONST UNDO_BIDX% = 3
CONST COL_WIDTH% = MM.INFO(FONTWIDTH)
CONST ROW_HEIGHT% = MM.INFO(FONTHEIGHT)
CONST WIN_BORDER_W% = COL_WIDTH%
CONST WIN_BORDER_H% = ROW_HEIGHT%
CONST FULL_SCREEN_WINDOW_X% = 0
CONST FULL_SCREEN_WINDOW_Y% = 0
CONST FULL_SCREEN_WINDOW_W% = MM.HRES
CONST FULL_SCREEN_WINDOW_H% = MM.VRES - 2*ROW_HEIGHT%
CONST CURSOR_BLINK_PERIOD% = 500
CONST PROMPTY% = MM.VRES-2*ROW_HEIGHT%

'--> Undo operation codes:
CONST UNDO_DELETE_SELECTION% = 1
CONST UNDO_ENTER% = 2
CONST UNDO_DELETE% = 3
CONST UNDO_BACKSPACE% = 4
CONST UNDO_EDIT% = 5
CONST UNDO_PASTE% = 6
CONST UNDO_INDENT% = 7
CONST UNDO_UNINDENT% = 8
'<-- Undo operation codes.

CONST LCTRL_MASK% = 2
CONST RCTRL_MASK% = 32
CONST LALT_MASK% = 1
CONST RALT_MASK% = 16
CONST LSHFT_MASK% = 8
CONST RSHFT_MASK% = 128

'A keycode can be seen as a bit value with the regular keycode in the lower 8 bits, 
'and the Ctrl/Alt/Shift modifiers in the bit positions given here
CONST KEYCODE_CTRL_BITPOS% = 8
CONST KEYCODE_ALT_BITPOS% = 9
CONST KEYCODE_SHFT_BITPOS% = 10

'Misc. constants
CONST HELP_SPRITE_ID% = 1
CONST CRSR_SPRITE_ID% = 2
CONST CRSR_MODE_INS% = 0
CONST CRSR_MODE_OVR% = 1
CONST SCREEN_CONTEXT_SIZE% = 7

'--> Window Split Mode values
CONST NO_SPLIT% = 0
CONST VSPLIT% = 1 'Vertical Split
CONST HSPLIT% = 2 'Horizontal Split
CONST NUM_SPLIT_MODES% = 3
'<--

'--> Macro recording data structures
DIM macroRecEnabled% = 0
DIM macroRecord%(MAX_NUM_MACRO_RECORDINGS%-1)
DIM macroRecordNumEntries% = 0

'<-- Macro recording

'--> Undo data structure
DIM undoAction%(MAX_NUM_UNDOS%-1) 'See UNDO_* constants
DIM undoSelStartRow%(MAX_NUM_UNDOS%-1) 'Undo cursor position or undo selection start area, as needed.
DIM undoSelStartCol%(MAX_NUM_UNDOS%-1)
DIM undoSelEndRow%(MAX_NUM_UNDOS%-1)   'Undo selection end area (if needed). 
DIM undoSelEndCol%(MAX_NUM_UNDOS%-1)
DIM undoBufStartRow%(MAX_NUM_UNDOS%-1) 'Pointer into a buffer holding text to restore.
DIM undoBufNumRows%(MAX_NUM_UNDOS%-1)
DIM undoIdx% 'Current position in the undo array
'<--
initUndo

'--> Buffer data structure
DIM bufLinePtrs%(MAX_NUM_ROWS%, NUM_BUFFERS%-1) 'Each entry is an index into theStrings pool below.
DIM bufNumRows%(NUM_BUFFERS%-1)
DIM bufFilename$(NUM_BUFFERS%-1)
DIM bufIsModified%(NUM_BUFFERS%-1)
DIM bufSavedCrsrCol%(NUM_BUFFERS%-1) 'Used to maintain cursor position when the buffer is not associated with a window.
DIM bufSavedCrsrRow%(NUM_BUFFERS%-1)
'<--
initAllBuffers

'--> Window data structure
DIM winX%(MAX_NUM_WINDOWS%-1) 'Outer upper left corner coordinates
DIM winY%(MAX_NUM_WINDOWS%-1)
DIM winContentX%(MAX_NUM_WINDOWS%-1) 'Inner upper left corner coordinates
DIM winContentY%(MAX_NUM_WINDOWS%-1)
DIM winNumRows%(MAX_NUM_WINDOWS%-1)
DIM winNumCols%(MAX_NUM_WINDOWS%-1)
DIM winWinCrsrRow%(MAX_NUM_WINDOWS%-1) 'Position of cursor in the window
DIM winWinCrsrCol%(MAX_NUM_WINDOWS%-1)
DIM winBufCrsrRow%(MAX_NUM_WINDOWS%-1) 'Position of cursor in the buffer
DIM winBufCrsrCol%(MAX_NUM_WINDOWS%-1)
DIM winBufCrsrTargetCol%(MAX_NUM_WINDOWS%-1) 'Target col to try to maintain when scrolling down.
DIM winBufTopRow%(MAX_NUM_WINDOWS%-1) 'Row in the buffer corresponding to the first line in the window.
DIM winBufTopCol%(MAX_NUM_WINDOWS%-1) 'Col                                          col.
DIM winSelectRow%(MAX_NUM_WINDOWS%-1) 'Holds selection start point.
DIM winSelectCol%(MAX_NUM_WINDOWS%-1)  
DIM winW%(MAX_NUM_WINDOWS%-1) 'Width
DIM winH%(MAX_NUM_WINDOWS%-1) 'Height
DIM winRequestRedraw%(MAX_NUM_WINDOWS%-1)
DIM winVisible%(MAX_NUM_WINDOWS%-1) 'True is window is visible.
DIM winBuf%(MAX_NUM_WINDOWS%-1) 'Associated buffer.
'<--
initAllWindows

'Used by string allocator. Each bit corresponds to one string in theStrings array below
DIM allocatorBitArray%(MAX_NUM_ROWS%/64)

'A pool of strings used by the buffer accessors. The buffers themselves don't hold the actual strings.
'They hold indices to this string pool. Strings must be allocated to the buffer before use, and released when no longer needed.
DIM theStrings$(MAX_NUM_ROWS%)

'--> Cursor Sprite definitions
DIM CRSR_INS_SPRITE%(COL_WIDTH%*ROW_HEIGHT%-1) 'Insert cursor sprite
CRSR_INS_SPRITE_DATA: 
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0 ,0 ,0

SUB initInsSprite
  LOCAL ii%

  RESTORE CRSR_INS_SPRITE_DATA
  FOR ii%=0 TO COL_WIDTH%*ROW_HEIGHT%-1
      READ CRSR_INS_SPRITE%(ii%) 
  NEXT ii%
END SUB

initInsSprite

DIM CRSR_OVR_SPRITE%(COL_WIDTH%*ROW_HEIGHT%-1) 'Overwrite cursor sprite
CRSR_OVR_SPRITE_DATA:
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA 0, 0, 0, 0, 0 ,0 ,0 ,0
  DATA FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%
  DATA FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%, FG_COLOR2%

SUB initOvrSprite
  LOCAL ii%

  RESTORE CRSR_OVR_SPRITE_DATA
  FOR ii%=0 TO COL_WIDTH%*ROW_HEIGHT%-1
    READ CRSR_OVR_SPRITE%(ii%) 
  NEXT ii%
END SUB

initOvrSprite
'<--

DIM showKeyCodeAtPrompt% = 0
DIM keyCounter% = 0
DIM exitRequested% = 0
DIM splitMode% = NO_SPLIT%
DIM strToFind$ = "" 'For the find function.

DIM crsrMode% = CRSR_MODE_INS%  'Insert or overwrite mode
DIM crsrActiveWidx% = 0 'Active window index.
DIM crsrState% = 0 '1 or 0, On or Off.

'--> CSUBS. Two functions I couldn't get fast enough in MMBasic, used when inserting/deleting lines.
'void moveBlockDown(long long *from, long long *to, long long *numElemsp) {
' long long *endp = from + *numElemsp;
'
' while (from < endp) {
'   *to = *from;
'   ++to;
'   ++from;
' }
'}
CSUB moveBlockDown INTEGER, INTEGER, INTEGER
  00000000
  B087B480 60F8AF00 607A60B9 E9D3687B 46132300 68FA00DB 617B4413 68FBE00B 
  2300E9D3 E9C168B9 68BB2300 60BB3308 330868FB 68FA60FB 429A697B BF00D3EF 
  371CBF00 F85D46BD 47707B04 
End CSUB

'void moveBlockUp(long long *from, long long *to, long long *numElemsp) {
' long long numElems = *numElemsp;
' long long *fptr = from + numElems - 1;
' long long *tptr = to + numElems - 1;
'
' while (fptr >= from) {
'   *tptr = *fptr;
'   --tptr;
'   --fptr;
' }
'}
CSUB moveBlockUp INTEGER, INTEGER, INTEGER
  00000000
  B089B480 60F8AF00 607A60B9 E9D3687B E9C72300 693A2304 4360F06F 00DB4413 
  441368FA 693A61FB 4360F06F 00DB4413 441368BA E00B61BB E9D369FB 69B92300 
  2300E9C1 3B0869BB 69FB61BB 61FB3B08 68FB69FA D2EF429A BF00BF00 46BD3724 
  7B04F85D 43474770 
End CSUB
'<-- CSUBS

PAGE WRITE 1
COLOUR FG_COLOR%, BG_COLOR%
CLS
PAGE WRITE 0
COLOUR FG_COLOR%, BG_COLOR%
CLS

setCrsrSprite

drawGenFooter
initWindow 0, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%, 0
drawWindow 0

SUB setupCtxt
  LOCAL dummy%, bIdx%
  
  IF MM.CMDLINE$ <> "" THEN
    dummy% = checkAndLoad%(0, MM.CMDLINE$) 'File to edit can be passed in on command line.
    drawWindow 0
  ELSE
    IF RESTORE_PREV_SESSION_CTXT% THEN
      restoreSessionCtxt
      IF bufFilename$(0) <> "" THEN
        dummy% = checkAndLoad%(0, bufFilename$(0))
      ENDIF
      IF bufFilename$(1) <> "" THEN
        dummy% = checkAndLoad%(1, bufFilename$(1))
      ENDIF
      
      bIdx% = winBuf%(crsrActiveWidx%)            
      gotoBufPos bufSavedCrsrRow%(bIdx%), bufSavedCrsrCol%(bIdx%), 0, 1
  
      drawWindow 0
    ENDIF
  ENDIF
END SUB
setupCtxt

DIM blinkCursorFlag% = 0
settick CURSOR_BLINK_PERIOD%, blinkCursorInt, 1

mainLoop

EndOfProg:
IF RESTORE_PREV_SESSION_CTXT% THEN
  saveSessionCtxt
ENDIF

CLS RGB(BLACK)
CLEAR
END

SUB mainLoop
  LOCAL wIdx%

  DO WHILE exitRequested% = 0
    drawGenFooter 'Always redraw because it has status info.

    IF blinkCursorFlag% THEN
      blinkCursor
      blinkCursorFlag% = 0
    ENDIF

    checkKeyAndModifier 'Input key handling. The heart of the editor.

    'Some logic to check if a window needs to be redrawn as requested by one of the key handling routines.
    FOR wIdx% = 0 TO MAX_NUM_WINDOWS%-1
      IF winVisible%(wIdx%) THEN
        drawWinHeader wIdx%
        IF winRequestRedraw%(wIdx%) THEN
          drawWinContents wIdx%
          winRequestRedraw%(wIdx%) = 0
        ENDIF
      ENDIF
    NEXT wIdx%

    crsrDraw
  LOOP  
END SUB

SUB restoreSessionCtxt
  LOCAL lin$

  IF DIR$(CTXT_FILE_PATH$) <> "" THEN
    OPEN CTXT_FILE_PATH$ FOR INPUT AS #1
    
    LINE INPUT #1, lin$
    bufFilename$(0) = lin$
  
    LINE INPUT #1, lin$  
    bufSavedCrsrCol%(0) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufSavedCrsrRow%(0) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufFilename$(1) = lin$
    
    LINE INPUT #1, lin$
    bufSavedCrsrCol%(1) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufSavedCrsrRow%(1) = VAL(lin$)

    LINE INPUT #1, lin$
    winBuf%(crsrActiveWidx%) = VAL(lin$)
    
    'Set up the other one while we're at it.
    winBuf%(NOT crsrActiveWidx%) = NOT winBuf%(crsrActiveWidx%)
    
    CLOSE #1
  ENDIF
END SUB

SUB saveSessionCtxt
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)

  'Save active window's cursor position (the other one is already saved.
  bufSavedCrsrCol%(bIdx%) = winBufCrsrCol%(crsrActiveWidx%)
  bufSavedCrsrRow%(bIdx%) = winBufCrsrRow%(crsrActiveWidx%)

  OPEN CTXT_FILE_PATH$ FOR OUTPUT AS #1
  
  PRINT #1, bufFilename$(0)
  PRINT #1, STR$(bufSavedCrsrCol%(0))
  PRINT #1, STR$(bufSavedCrsrRow%(0))       
  PRINT #1, bufFilename$(1)
  PRINT #1, STR$(bufSavedCrsrCol%(1))
  PRINT #1, STR$(bufSavedCrsrRow%(1))
  PRINT #1, STR$(winBuf%(crsrActiveWidx%))
  
  CLOSE #1
END SUB

'help popup is prepared on a separate page in a Box, then shown on page 0 using a sprite.
SUB showHelpPopup                                                                
  LOCAL longestStringLen% = LEN("Key Bindings (Ref. Key Bindings section in XEdit.bas to modify):")
  
  LOCAL numLines% = 33
  LOCAL boxWidth% = (longestStringLen%+4)*COL_WIDTH%
  LOCAL boxHeight% = (numLines%+4)*ROW_HEIGHT%

  PAGE WRITE 2
  BOX 0, 0, boxWidth%, boxHeight%, 4, RGB(RED), RGB(WHITE)
  
  LOCAL x% = 2*COL_WIDTH%
  LOCAL y% = 2*ROW_HEIGHT%

  LOCAL title$ = "XEdit Help"
  PRINT @(x%,y%,2) SPACE$((longestStringLen% - LEN(title$))\2) + title$;
  y% = y% + 2*ROW_HEIGHT%

  PRINT @(x%,y%,2) "Key Bindings (Ref. Key Bindings section in XEdit.bas to modify):";
  Y% = Y% + 2*ROW_HEIGHT%                  
  PRINT @(x%,y%,2) "F1         = Help";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F2/F9      = Save File/Save File as";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F3         = Load File";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F12        = Close File";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F4         = Toggle Buffer";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F5         = Toggle Window split";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F10        = Quit";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Ctrl-O     = Toggle Active Window";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Ctrl-F     = Find Prompt/Selection";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Alt-F      = Find Next";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Ctrl-R     = Replace Prompt/Selection";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Ctrl-X/Y/V = Cut/Copy/Paste";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Ctrl-G     = Goto Line";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "INS        = Toggle Insert/Overwrite mode";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Home 1x/2x/3x = Go To Start of Line/Page/Buffer";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "End 1x/2x/3x = Go To End of Line/Page/Buffer";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Tab/Shift-Tab = Indent/Unindent Line/Selection";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Shift-Navigation Key = Start/Extend Selection";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Ctrl-Z     = Undo";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F7         = Start Macro Recording";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "F8         = Stop Macro Recording / Playback recorded macro";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Alt-K      = Show Key Code at prompt";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "Alt-S      = Screenshot";

  Y% = Y% + 2*ROW_HEIGHT%
  PRINT @(x%,y%,2) "User Configurable Settings (Set at start of XEdit.bas):";
  Y% = Y% + 2*ROW_HEIGHT%
  PRINT @(x%,y%,2) "SEARCH_IS_CASE_SENSITIVE%=0/1. Default=0.";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "TAB_WIDTH%=<Num.>. Default=2.";
  Y% = Y% + ROW_HEIGHT%
  PRINT @(x%,y%,2) "RESTORE_PREV_SESSION_CTXT%=0/1. Default=1.";

  PAGE WRITE 0

  SPRITE READ HELP_SPRITE_ID%, 0 , 0, boxWidth%, boxHeight%, 2
  SPRITE SHOW HELP_SPRITE_ID%, MM.HRES/2 - boxWidth%/2, MM.VRES/2 - boxHeight%/2, 1
END SUB

SUB removeHelpPopup
  SPRITE CLOSE HELP_SPRITE_ID%
END SUB

'--> Save current position of buffer in window and cursor in window so we can go back to it
'Note that this only works correctly in window dimensions for save and restore point are the same.
SUB saveScreenPos(screenContext%())
  screenContext%(0) = winWinCrsrRow%(crsrActiveWidx%)
  screenContext%(1) = winWinCrsrCol%(crsrActiveWidx%)
  screenContext%(2) = winBufCrsrRow%(crsrActiveWidx%)
  screenContext%(3) = winBufCrsrCol%(crsrActiveWidx%)
  screenContext%(4) = winBufCrsrTargetCol%(crsrActiveWidx%)
  screenContext%(5) = winBufTopRow%(crsrActiveWidx%)
  screenContext%(6) = winBuftopCol%(crsrActiveWidx%)
END SUB

SUB restoreScreenPos(screenContext%())
  winWinCrsrRow%(crsrActiveWidx%) = screenContext%(0)
  winWinCrsrCol%(crsrActiveWidx%) = screenContext%(1)
  winBufCrsrRow%(crsrActiveWidx%) = screenContext%(2)
  winBufCrsrCol%(crsrActiveWidx%) = screenContext%(3)
  winBufCrsrTargetCol%(crsrActiveWidx%) = screenContext%(4)
  winBufTopRow%(crsrActiveWidx%) = screenContext%(5)
  winBuftopCol%(crsrActiveWidx%) = screenContext%(6)
END SUB
'<--

'Return true if given character is printable
FUNCTION isPrintable%(char%)
  isPrintable% = (char% >= 32) AND (char% <= 255)
END FUNCTION

FUNCTION findCharAtCrsr$()
  LOCAL bufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL bufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$

  'We might be at the end of the line or past the last row. Use a space character in that case.
  IF bufCrsrRow% > = bufNumRows%(bIdx%) THEN
    findCharAtCrsr$ = " "
    EXIT SUB
  ENDIF

  rdBufLine(bIdx%, bufCrsrRow%, lin$)

  IF bufCrsrCol% >= LEN(lin$) THEN
    findCharAtCrsr$ = " "
    EXIT SUB
  ENDIF

  findCharAtCrsr$ = MID$(lin$, bufCrsrCol%+1, 1)  
END FUNCTION

'The IRQ just sets the flag, which'll cause blinkCursor below to be called from the main loop.
SUB blinkCursorInt
  blinkCursorFlag% = 1
END SUB

'This SUB is called from the mainLoop.
SUB crsrDraw
  IF crsrState% THEN
    STATIC prevCrsrY% = 0
    STATIC prevCrsrX% = 0

    LOCAL crsrY% = winWinCrsrRow%(crsrActiveWidx%)*ROW_HEIGHT% + winContentY%(crsrActiveWidx%) 
    LOCAL crsrX% = winWinCrsrCol%(crsrActiveWidx%)*COL_WIDTH% + winContentX%(crsrActiveWidx%)

    'Only do a sprite show when position moved.
    IF (crsrY% <> prevCrsrY%) OR (crsrX% <> prevCrsrX%) THEN
      SPRITE SHOW CRSR_SPRITE_ID%, crsrX%, crsrY%, 1, 0
      prevCrsrY% = crsrY%
      prevCrsrX% = crsrX%
    ENDIF
  ENDIF
END SUB

SUB crsrOn
  IF NOT crsrState% THEN
    LOCAL crsrY% = winWinCrsrRow%(crsrActiveWidx%)*ROW_HEIGHT% + winContentY%(crsrActiveWidx%) 
    LOCAL crsrX% = winWinCrsrCol%(crsrActiveWidx%)*COL_WIDTH% + winContentX%(crsrActiveWidx%)

    SPRITE SHOW CRSR_SPRITE_ID%, crsrX%, crsrY%, 1, 0
    crsrState% = 1
  ENDIF
END SUB

SUB crsrOff
  IF crsrState% THEN
    ON ERROR SKIP 1
      SPRITE HIDE CRSR_SPRITE_ID%
    crsrState% = 0
  ENDIF
END SUB

FUNCTION crsrDisable%()
  crsrDisable% = crsrState%
  crsrOff
END FUNCTION

SUB crsrRestore(state%)
  IF state% THEN
    crsrOn
  ELSE
    crsrOff
  ENDIF
END SUB

SUB blinkCursor
  STATIC crsrOnOff%=0

  crsrOnOff% = NOT crsrOnOff%

  IF crsrOnOff% THEN
    crsrOn
  ELSE
    crsrOff
  ENDIF
END SUB

SUB setCrsrSprite
  ON ERROR SKIP 1
    SPRITE CLOSE CRSR_SPRITE_ID%

  IF crsrMode% = CRSR_MODE_INS% THEN
    SPRITE LOADARRAY CRSR_SPRITE_ID%, COL_WIDTH%, ROW_HEIGHT%, CRSR_INS_SPRITE%()
  ELSE
    SPRITE LOADARRAY CRSR_SPRITE_ID%, COL_WIDTH%, ROW_HEIGHT%, CRSR_OVR_SPRITE%()
  ENDIF
END SUB

'--> allocatorBitArray bit array accessor functions:
'Mark string specified by offset as allocated (=1) or not free (=0)
SUB setAllocated(offset%, allocated%)
  LOCAL idx% = offset%>>6
  LOCAL bitpos% = 1<<(offset% AND 63)

  IF allocated% THEN
    allocatorBitArray%(idx%) = allocatorBitArray%(idx%) OR bitpos%
  ELSE
    allocatorBitArray%(idx%) = allocatorBitArray%(idx%) AND (-1 XOR bitpos%)
  ENDIF  
END SUB

'Returns true if position specified by offset is flagged as allocated.
FUNCTION isAllocated%(offset%)
  isAllocated% = allocatorBitArray%(offset%>>6) AND (1<<(offset% AND 63))
END FUNCTION
'<--

'lineId is an output argument
FUNCTION allocateLine%(lineId%)
  'We maintain a previous lineId from which we start searching for an available line.
  'This typically gives a faster result than starting the search from 0 each time.
  STATIC prevlineId% = 0
  LOCAL ii% = prevLineId% + 1, bitpos%, idx%

  IF ii% >= MAX_NUM_ROWS% THEN
    ii%=0
  ENDIF

  'The allocator bit accessors have been inline here for performance reasons.
  DO WHILE ii% <> prevLineId%
    bitpos% = 1<<(ii% AND 63)
    idx% = ii%>>6
    IF (allocatorBitArray%(idx%) AND bitpos%)=0 THEN
      allocatorBitArray%(idx%) = allocatorBitArray%(idx%) OR bitpos%
      lineId% = ii%
      prevLineId% = ii%
      allocateLine% = 1
      EXIT FUNCTION 
    ENDIF

    ii% = ii% + 1
    IF ii% >= MAX_NUM_ROWS% THEN
      ii%=0
    ENDIF
  LOOP

  allocateLine% = 0
END FUNCTION

'lineId is an output argument
SUB freeLine(linePtr%)
  IF linePtr% <> -1 THEN
    setAllocated(linePtr%, 0)
    linePtr% = -1
  ENDIF
END SUB

'<-- Buffer accessors
'Delete a block of lines at given position.
SUB deleteBufLines(bIdx%, row%, numRows%)
  IF (numRows% = 0) THEN
    EXIT SUB
  ENDIF

  LOCAL rowl%
  FOR rowl%=row% TO row%+numRows%-1
    freeLine bufLinePtrs%(rowl%, bIdx%)
  NEXT rowl%

  moveBlockDown bufLinePtrs%(row%+numRows%, bIdx%), bufLinePtrs%(row%, bIdx%), bufNumRows%(bIdx%)-numRows%

  bufNumRows%(bIdx%) = bufNumRows%(bIdx%) - numRows%
  bufIsModified%(bIdx%) = 1
END SUB

'Insert a block of lines at given position. Note that the lines' contents are not give here. It expected that
'the newly created lines will be written by wrBufLine shortly after the insertBufLines call.
FUNCTION insertBufLines%(bIdx%, row%, numRows%)
  IF bufNumRows%(bIdx%)+numRows% >= MAX_NUM_ROWS% THEN
    promptMsg "Buffer full!", 1
    insertBufLines% = 0
    EXIT FUNCTION
  ENDIF

  moveBlockUp bufLinePtrs%(row%, bIdx%), bufLinePtrs%(row%+numRows%, bIdx%), bufNumRows%(bIdx%)-row%

  LOCAL rowl%
  LOCAL lastRow% = row%+numRows%-1
  FOR rowl% = row% TO lastRow%
    bufLinePtrs%(rowl%, bIdx%) = -1
  NEXT rowl%

  bufNumRows%(bIdx%) = bufNumRows%(bIdx%) + numRows%
  bufIsModified%(bIdx%) = 1
  insertBufLines% = 1
END FUNCTION

'Write a line into buffer bIdx, allocate line if needed. Returns false if failed.
FUNCTION wrBufLine%(bIdx%, row%, lin$)
  LOCAL linePtr% = bufLinePtrs%(row%, bIdx%)
  IF linePtr% = -1 THEN
    IF NOT allocateLine%(linePtr%) THEN
      promptMsg "Not Enough Memory!", 1
      wrBufLine% = 0
      EXIT FUNCTION
    ENDIF
  ENDIF

  'Maintain the buffer's number of rows here.
  IF bufNumRows%(bIdx%) <= row% THEN
    bufNumRows%(bIdx%) = row% + 1
  ENDIF

  theStrings$(linePtr%) = lin$
  bufLinePtrs%(row%, bIdx%) = linePtr%
  bufIsModified%(bIdx%) = 1
  wrBufLine% = 1
END FUNCTION

'Read one line for given buffer at given position.
SUB rdBufLine(bIdx%, row%, lin$)
  LOCAL linePtr% = bufLinePtrs%(row%, bIdx%)
  IF (linePtr% <> -1) THEN
    IF isAllocated%(linePtr%) THEN
      lin$ = theStrings$(linePtr%)
    ENDIF
  ELSE
    lin$ = ""
  ENDIF
END SUB
'-->

SUB initUndo
  LOCAL ii%
  FOR ii% = 0 TO MAX_NUM_UNDOS%-1
    undoAction%(ii%) = 0
    undoSelStartRow%(ii%) = 0
    undoSelStartCol%(ii%) = 0
    undoSelEndRow%(ii%) = 0
    undoSelEndCol%(ii%) = 0
    undoBufStartRow%(ii%) = 0
    undoBufNumRows%(ii%) = 0
  NEXT ii%
  undoIdx% = 0
END SUB

SUB initAllBuffers
  LOCAL bIdx%

  FOR bIdx% = 0 TO NUM_BUFFERS%-1
    initBuffer bIdx%
  NEXT bIdx%
END SUB

SUB initBuffer(bIdx%)
  LOCAL ii% 
  bufNumRows%(bIdx%) = 1
  bufFilename$(bIdx%) = ""
  bufIsModified%(bIdx%) = 0
  bufSavedCrsrCol%(bIdx%) = 0
  bufSavedCrsrRow%(bIdx%) = 0

  FOR ii%=0 TO MAX_NUM_ROWS%-1
    bufLinePtrs%(ii%, bIdx%) = -1
  NEXT ii%
END SUB

SUB setupBuffer(bIdx%, numRows%, filename$) 
  bufNumRows%(bIdx%) = numRows%
  bufFilename$(bIdx%) = filename$
  bufIsModified%(bIdx%) = 0
END SUB

'Empty the keyboard input buffer
SUB emptyInputBuffer
  DO WHILE KEYDOWN(1) <> 0
  LOOP
END SUB

'If on%=1, prompt text is shown. If on%=0 prompt text is removed.
SUB promptMsg(text$, on%)
  IF on%=1 THEN
    PRINT @(0,PROMPTY%) text$ + SPACE$(MM.HRES\COL_WIDTH% - LEN(text$));
    emptyInputBuffer
  ELSE
    PRINT @(0,PROMPTY%) SPACE$(MM.HRES\COL_WIDTH%);
  ENDIF
END SUB

'Prints the given text on the prompt line, then waits for the user to press any key. 
'The pressed key is returned to the caller.
FUNCTION promptForAnyKey$(text$)
  LOCAL pressedKey$
  LOCAL latchedTime% = INT(TIMER)

  PRINT @(0,PROMPTY%) text$ + SPACE$(FULL_SCREEN_WINDOW_W%\COL_WIDTH% - LEN(text$));
  LOCAL crsrPos% = (LEN(text$)+1)*COL_WIDTH%
  LOCAL invert% = 0

  emptyInputBuffer

  'An overly complex way of getting a blinking cursor at the prompt...
  DO 
    pressedKey$ = INKEY$ 
    PRINT @(crsrPos%, PROMPTY%, invert%) " ";
    IF (INT(TIMER) > latchedTime% + CURSOR_BLINK_PERIOD%) THEN
      invert% = invert% XOR 2
      latchedTime% = INT(TIMER)
    ENDIF
  LOOP UNTIL pressedKey$ <> ""

  PRINT @(0,PROMPTY%) SPACE$(MM.HRES\COL_WIDTH%);

  emptyInputBuffer

  promptForAnyKey$ = pressedKey$
END FUNCTION

'Prints the given text on the prompt line, then waits for input. 
'The input string is returned to the caller.
FUNCTION promptForText$(text$)
  LOCAL inputStr$
  PRINT @(0,PROMPTY%) text$;
  INPUT "", inputStr$
  emptyInputBuffer
  PRINT @(0,PROMPTY%) SPACE$(MM.HRES\COL_WIDTH%);
  promptForText$ = inputStr$  
END FUNCTION

'Load file into buffer. Returns number of rows loaded. 0 if failed.
FUNCTION loadFile%(filename$, bIdx%)
  LOCAL row% = 0
  LOCAL lin$

  OPEN filename$ FOR INPUT AS #1

  promptMsg "Loading...", 1

  DO WHILE NOT EOF(#1)
    LINE INPUT #1, lin$
    IF NOT wrBufLine%(bIdx%, row%, lin$) THEN
      loadFile% = 0
      EXIT FUNCTION
    ENDIF
    row% = row% + 1
  LOOP

  CLOSE #1
  promptMsg "", 0
  bufIsModified%(bIdx%) = 0
  resetUndo 0
  loadFile% = row%
END FUNCTION

'Returns true if OK, false if load aborted
FUNCTION checkAndLoad%(bIdx%, fileToLoad$)
  LOCAL numRows%
  LOCAL fileToLoadl$ = fileToLoad$
  LOCAL fileIsValid% = 0
  LOCAL fileSizel%
  LOCAL ii%

  IF fileToLoadl$ = "" THEN
    checkAndLoad% = 0
    EXIT FUNCTION
  ENDIF

  fileSizel% = MM.INFO(FILESIZE fileToLoadl$)  

  IF fileSizel% = -1 THEN
    IF UCASE$(promptForAnyKey$("File " + fileToLoadl$ + " not found. Create File? (Y/N)")) <> "Y" THEN
      checkAndLoad% = 0
      EXIT FUNCTION
    ENDIF
    'A new, empty file
    FOR ii%=0 TO bufNumRows%(bIdx%)-1
      freeLine bufLinePtrs%(ii%, bIdx%)
    NEXT ii%
    numRows% = 0
  ELSE 'Existing file:
    FOR ii%=0 TO bufNumRows%(bIdx%)-1
      freeLine bufLinePtrs%(ii%, bIdx%)
    NEXT ii%
    numRows% = loadFile%(fileToLoadl$, bIdx%)
    IF numRows% = 0 THEN
      checkAndLoad% = 0
      EXIT FUNCTION
    ENDIF
  ENDIF

  setupBuffer bIdx%, numRows%, fileToLoad$
  checkAndLoad% = 1
END FUNCTION

SUB initAllWindows
  LOCAL wIdx%
  FOR wIdx% = 0 TO MAX_NUM_WINDOWS% - 1
    initWindow wIdx%, 0, 0, 0 ,0, 0
  NEXT wIdx%
END SUB

SUB resetWindow(wIdx%)
  winWinCrsrRow%(wIdx%) = 0
  winWinCrsrCol%(wIdx%) = 0
  winBufCrsrRow%(wIdx%) = 0
  winBufCrsrCol%(wIdx%) = 0
  winBufCrsrTargetCol%(wIdx%) = 0
  winBufTopRow%(wIdx%) = 0
  winBufTopCol%(wIdx%) = 0
END SUB

SUB initWindow(wIdx%, x%, y%, w%, h%, bIdx%)
  resizeWindow wIdx%, x%, y%, w%, h%
  winSelectRow%(wIdx%) = -1
  winSelectCol%(wIdx%) = -1
  winBuf%(wIdx%) = bIdx%
  winRequestRedraw%(wIdx%) = 0
  resetWindow wIdx%
END SUB

SUB resizeWindow(wIdx%, x%, y%, w%, h%)
  winX%(wIdx%) = x%
  winY%(wIdx%) = y%
  winW%(wIdx%) = w%
  winH%(wIdx%) = h%
  winContentX%(wIdx%) = x% + WIN_BORDER_W%
  winContentY%(wIdx%) = y% + WIN_BORDER_H%
  winNumCols%(wIdx%) = (w% - 2*WIN_BORDER_W%)\COL_WIDTH% 
  winNumRows%(wIdx%) = (h% - 2*WIN_BORDER_H%)\ROW_HEIGHT%
  winVisible%(wIdx%) = (w%>0) OR (h%>0) 
END SUB

FUNCTION winColToXpos%(wIdx%, winCol%)
  winColToXpos% = winContentX%(wIdx%) + winCol%*COL_WIDTH%
END FUNCTION

FUNCTION winRowToYpos%(wIdx%, winRow%)
  winRowToYpos% = winContentY%(wIdx%) + winRow%*ROW_HEIGHT%
END FUNCTION

'When text is selected the cursor maybe before or after the selection
'start point. This sub puts the lowest coordinates in startRow/Col
'and the highest in endRow/Col.
SUB selectionBoundaries(wIdx%, startRow%, startCol%, endRow%, endCol%)
  IF selectMode%(wIdx%) THEN
    startRow% = MIN(winSelectRow%(wIdx%), winBufCrsrRow%(wIdx%))
    endRow% = MAX(winSelectRow%(wIdx%), winBufCrsrRow%(wIdx%))
    IF startRow% = endRow% THEN
      startCol% = MIN(winSelectCol%(wIdx%), winBufCrsrCol%(wIdx%))
      endCol% = MAX(winSelectCol%(wIdx%), winBufCrsrCol%(wIdx%))
    ELSE
      IF startRow% = winSelectRow%(wIdx%) THEN
        startCol% = winSelectCol%(wIdx%)
        endCol% = winBufCrsrCol%(wIdx%)
      ELSE
        startCol% = winBufCrsrCol%(wIdx%)
        endCol% = winSelectCol%(wIdx%)
      ENDIF
    ENDIF
  ELSE
    startRow% = -1
    startCol% = -1
    endRow% = -1
    endCol% = -1
  ENDIF
END SUB

'Convert buffer column to window column taking into account the horizontal
'offset of the window into the buffer (winBufTopCol).
FUNCTION bufToWinCol%(wIdx%, bufCol%)
  bufToWinCol% = MIN(MAX(bufCol% - winBufTopCol%(wIdx%), 0), winNumCols%(wIdx%))
END FUNCTION

'Returns true when a selection is active on given window.
FUNCTION selectMode%(wIdx%)
  selectMode% = (winSelectRow%(wIdx%) <> -1)
END FUNCTION

'Draw one text row in the given window at the given position.
'checkOtherWin specified to check if the other window needs a
'redraw too (it may be looking at the same text).
SUB drawWinRow(wIdx%, winRow%, checkOtherWin%)
  LOCAL bIdx% = winBuf%(wIdx%)
  LOCAL bufLine$ = ""
  LOCAL bufRow% = winBufTopRow%(wIdx%) + winRow%
  rdBufLine(bIdx%, bufRow%, bufLine$)

  LOCAL x% = winColToXpos%(wIdx%, 0)
  LOCAL y% = winRowToYpos%(wIdx%, winRow%)
  LOCAL strToPrint$ = MID$(bufLine$, winBufTopCol%(wIdx%)+1, winNumCols%(wIdx%))
  LOCAL spacetoAppend$ = SPACE$(winNumCols%(wIdx%) - LEN(strToPrint$))
  LOCAL selStartRow%, selStartCol%, selEndRow%, selEndCol%

  selectionBoundaries(wIdx%, selStartRow%, selStartCol%, selEndRow%, selEndCol%)

  'Is a selection active on this line?
  IF (bufRow% >= selStartRow%) AND (bufRow% <= selEndRow%) THEN 'Selection active on this line
    'Four cases:
    IF (selStartRow% = selEndRow%) THEN '1. Selection starts and ends on current line
      TEXT x%, y%, strToPrint$ + spaceToAppend$
      selStartCol% = bufToWinCol%(wIdx%, selStartCol%)
      selEndCol% = bufToWinCol%(wIdx%, selEndCol%)
      IF selEndCol% > selStartCol% THEN 
        TEXT winColToXpos%(wIdx%,selStartCol%), y%, MID$(strToPrint$, selStartCol%+1, selEndCol% - selStartCol%),,,, FG_COLOR%, BG_COLOR2%
      ENDIF
    ELSEIF (bufRow% = selStartRow%) THEN '2. Selection starts on current line, ends on a different line.
      TEXT x%, y%, strToPrint$ + spaceToAppend$
      selStartCol% = bufToWinCol%(wIdx%, selStartCol%)
      selEndCol% = winNumCols%(wIdx%)
      IF selEndCol% > selStartCol% THEN
        TEXT winColToXpos%(wIdx%,selStartCol%), y%, MID$(strToPrint$, selStartCol%+1, selEndCol% - selStartCol%),,,, FG_COLOR%, BG_COLOR2% 
      ENDIF
    ELSEIF (bufRow% = selEndRow%) THEN '3. Selection started on a different line, ends on current line.
      TEXT x%, y%, strToPrint$ + spaceToAppend$
      selStartCol% = 0
      selEndCol% = bufToWinCol%(wIdx%, selEndCol%)
      IF selEndCol% > selStartCol% THEN 
        TEXT winColToXpos%(wIdx%,selStartCol%), y%, MID$(strToPrint$, selStartCol%+1, selEndCol% - selStartCol%),,,, FG_COLOR%, BG_COLOR2%
      ENDIF
      COLOR FG_COLOR%, BG_COLOR%
    ELSE '4. Selection starts and ends on a different line.
      TEXT x%, y%, strToPrint$ + spaceToAppend$
      TEXT x%, y%, strToPrint$,,,, FG_COLOR%, BG_COLOR2% 
    ENDIF
  ELSE 'No selection active.
    TEXT x%, y%, strToPrint$ + spaceToAppend$
  ENDIF

  IF checkOtherWin% THEN
    LOCAL otherWidx% = NOT wIdx%
    LOCAL otherBidx% = winBuf%(otherWidx%)
    LOCAL otherTopRow% = winBufTopRow%(otherWidx%)
    'Is this row visible in the other window?
    IF (otherBidx% = bIdx%) AND (bufRow% >= otherTopRow%) AND (bufRow% < otherTopRow% + winNumRows%(otherWidx%)) THEN
      drawWinRow otherWidx%, bufRow% - otherTopRow%, 0 'Then redraw that on too.
    ENDIF
  ENDIF
END SUB

'Draw all text rows in given window.
SUB drawWinContents(wIdx%)
  LOCAL winRow%
  LOCAL bIdx% = winBuf%(wIdx%)
  LOCAL crsrState%
  
  crsrState% = crsrDisable%()

  FOR winRow%=0 TO winNumRows%(wIdx%) - 1
    drawWinRow wIdx%, winRow%, 0
  NEXT winRow%

  crsrRestore crsrState%
END SUB

SUB drawWindow(wIdx%)
  BOX winX%(wIdx%), winY%(wIdx%), winW%(wIdx%), winH%(wIdx%), 2, FG_COLOR2%, BG_COLOR%
  drawWinContents wIdx%
END SUB

SUB drawWinHeader(wIdx%)
  LOCAL bIdx% = winBuf%(wIdx%)
  LOCAL modifiedIndicator$
  LOCAL filenamel$
  'Adding 1 to col and row so cursos position is presented as 1-based to user.
  LOCAL coordinates$ = STR$(winBufCrsrCol%(wIdx%)+1)+"/"+STR$(winBufCrsrRow%(wIdx%)+1)+"/"+STR$(bufNumRows%(bIdx%))
  LOCAL headerX% = winX%(wIdx%)
  LOCAL headerY% = winY%(wIdx%)
  LOCAL headerW% = winW%(wIdx%)

  IF bufIsModified%(bIdx%) THEN
    modifiedIndicator$ = " (M) "
  ELSE
    modifiedIndicator$ = "     "
  ENDIF

  IF bufFilename$(bIdx%) = "" THEN
    filenamel$ = "(...)" 'Just until we have a bufFilename.
  ELSE
    filenamel$ = bufFilename$(bIdx%)
  ENDIF

  'bIdx+1 so buffer 0 and 1 are presented as buffers 1 and 2 to user. Assume users are 1-based ;-)
  LOCAL headerLeft$ = " Buf." + STR$(bIdx%+1) + ", " 
  LOCAL headerCenter$ = filenamel$ + modifiedIndicator$
  LOCAL headerRight$ = coordinates$ + "  "
  LOCAL numSpaces% = headerW%\COL_WIDTH% - LEN(headerLeft$) - LEN(headerCenter$) - LEN(headerRight$)

  IF numSpaces% < 0 THEN
    'Shorten filename
    filenamel$ = "..." + RIGHT$(filenamel$, LEN(filenamel$) + numSpaces% - 3)
    headerCenter$ = filenamel$ + modifiedIndicator$
    numSpaces% = 0
  ENDIF 

  COLOR FG_COLOR2%
  'Print inverted.
  PRINT @(headerX%,headerY%,2) headerLeft$ + headerCenter$ + SPACE$(numSpaces%) + headerRight$;
  COLOR FG_COLOR%
END SUB

SUB drawGenFooter
  LOCAL headerX% = 0
  LOCAL headerY% = MM.VRES - ROW_HEIGHT% + 1
  LOCAL headerW% = FULL_SCREEN_WINDOW_W%
  LOCAL insOvrModeString$

  IF crsrMode% = CRSR_MODE_INS% THEN
    insOvrModeString$ = " (INS) "
  ELSE
    insOvrModeString$ = " (OVR) "
  ENDIF

  LOCAL headerLeft$ = "XEdit V"+VERSION$+" by Epsilon. " + insOvrModeString$
  LOCAL headerRight$ = "F1 = Help  "
  
  'Print inverted.
  PRINT @(headerX%,headerY%,2) headerLeft$ + SPACE$(headerW%\COL_WIDTH% - LEN(headerLeft$) - LEN(headerRight$)) + headerRight$;
END SUB

'Scroll horizontally to give offset from the left.
SUB scrollHoffset(wIdx%, fromLeft%)
  IF winBufTopCol%(wIdx%) <> fromLeft% THEN
    winBufTopCol%(wIdx%) = fromLeft%
    winRequestRedraw%(wIdx%) = 1
  ENDIF
END SUB

'Scroll horizontally a number of columns. Positive numCols% is scroll left.
SUB scrollHdelta(wIdx%, numCols%)
  winBufTopCol%(wIdx%) = winBufTopCol%(wIdx%) + numCols%
  winRequestRedraw%(wIdx%) = 1
END SUB

'Scroll vertically a number of rows. Positive numRows% is scroll down.
SUB scrollVdelta(wIdx%, numRows%)
  LOCAL bIdx% = winBuf%(widx%)
  winBufTopRow%(wIdx%) = winBufTopRow%(wIdx%) + numRows%
  winRequestRedraw%(wIdx%) = 1
END SUB

'--> Key Handler Section:
'------------------------

'Exit key handler.
SUB exitKeyHandler
  LOCAL bIdx%
  LOCAL anyFileModified% = 0

  FOR bIdx%=0 TO 1
    IF bufIsModified%(bIdx%) THEN
      anyFileModified% = 1
    ENDIF
  NEXT bIdx%

  IF anyFileModified% THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Are you sure you want to quit? (Y/N)")
    exitRequested% = (UCASE$(yesNo$)="Y")
  ELSE
    exitRequested% = 1
  ENDIF
END SUB

SUB toggleScreenSplitKeyHandler
  LOCAL ii%, bIdx%
  
  splitMode% = splitMode%+1

  IF splitMode% >= NUM_SPLIT_MODES% THEN
    splitMode% = 0
  ENDIF

  SELECT CASE splitMode%
    CASE NO_SPLIT% 'Currently active window only visible, full screen.
      resizeWindow crsrActiveWidx%, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%
      resizeWindow 1 - crsrActiveWidx%, 0, 0, 0 ,0
      drawWindow crsrActiveWidx%

    CASE VSPLIT%
      crsrOff
      resizeWindow 0, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%\2, FULL_SCREEN_WINDOW_H%
      resizeWindow 1, FULL_SCREEN_WINDOW_W%\2, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%\2, FULL_SCREEN_WINDOW_H%
      
      gotoBufPos winBufCrsrRow%(crsrActiveWidx%), winBufCrsrCol%(crsrActiveWidx%), 0, 1
      
      'Temp switch to other window to restore cursor position.
      toggleActiveWindowKeyHandler
      bIdx% = winBuf%(crsrActiveWidx%)            
      gotoBufPos bufSavedCrsrRow%(bIdx%), bufSavedCrsrCol%(bIdx%), 0, 1
      toggleActiveWindowKeyHandler
      
      drawWindow 0: drawWindow 1

    CASE HSPLIT%
      crsrOff
      resizeWindow 0, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%\2
      resizeWindow 1, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y% + FULL_SCREEN_WINDOW_H%\2, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%\2

      gotoBufPos winBufCrsrRow%(crsrActiveWidx%), winBufCrsrCol%(crsrActiveWidx%), 0, 1

      'Temp switch to other window to restore cursor position.
      toggleActiveWindowKeyHandler
      bIdx% = winBuf%(crsrActiveWidx%)            
      gotoBufPos bufSavedCrsrRow%(bIdx%), bufSavedCrsrCol%(bIdx%), 0, 1
      toggleActiveWindowKeyHandler
      
      drawWindow 0: drawWindow 1
  END SELECT
END SUB

SUB toggleActiveWindowKeyHandler
  LOCAL oob%
  
  IF splitMode% <> NO_SPLIT% THEN
    crsrActiveWidx% = crsrActiveWidx% + 1
    IF crsrActiveWidx% >= MAX_NUM_WINDOWS% THEN
      crsrActiveWidx% = 0
    ENDIF
    
    'Fix cursor if it's out of bounds.
    'oob% = winWinCrsrRow%(crsrActiveWidx%) - winNumRows%(crsrActiveWidx%) + 1
    'IF oob% > 0 THEN
    '  crsrUp oob%
    'ENDIF
    '
    'oob% = winWinCrsrCol%(crsrActiveWidx%) - winNumCols%(crsrActiveWidx%) + 1
    'IF oob% > 0 THEN
    '  crsrLeft oob%
    'ENDIF      
  ENDIF

  blinkCursor
END SUB

SUB toggleInsOvrModeKeyHandler
  crsrMode% = NOT crsrMode%
  setCrsrSprite
  blinkCursor
END SUB

SUB toggleBufferKeyHandler
  LOCAL ii%
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  
  'Save current buffer's cursor position
  bufSavedCrsrCol%(bIdx%) = winBufCrsrCol%(crsrActiveWidx%)
  bufSavedCrsrRow%(bIdx%) = winBufCrsrRow%(crsrActiveWidx%)

  bIdx% = NOT bIdx%
  winBuf%(crsrActiveWidx%) = bIdx%

  'Restore other buffer's cursor position
  gotoBufPos bufSavedCrsrRow%(bIdx%), bufSavedCrsrCol%(bIdx%), 1, 1
END SUB

SUB loadIntoCurrentBufKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)

  IF bufIsModified%(bIdx%) THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Discard the changes? (Y/N)")
    IF UCASE$(yesNo$) <> "Y" THEN
      EXIT SUB
     ENDIF
  ENDIF

  LOCAL filename$ = promptForText$("Load File: ")
  IF checkAndLoad%(winBuf%(crsrActiveWidx%), filename$) THEN
    resetWindow crsrActiveWidx%
    winRequestRedraw%(crsrActiveWidx%) = 1
    IF winBuf%(0) = winBuf%(1) THEN 'If the other window looks into the same buffer, reset that one too.
      resetWindow NOT crsrActiveWidx%
      winRequestRedraw%(NOT crsrActiveWidx%) = 1
    ENDIF
  ENDIF
END SUB

SUB closeBufferKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL ii%
  
  IF bufIsModified%(bIdx%) THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Discard the changes? (Y/N)")
    IF UCASE$(yesNo$) <> "Y" THEN
      EXIT SUB
     ENDIF
  ENDIF
  
  'A new, empty buffer
  FOR ii%=0 TO bufNumRows%(bIdx%)-1
    freeLine bufLinePtrs%(ii%, bIdx%)
  NEXT ii%

  setupBuffer bIdx%, 0, ""
  
  resetWindow crsrActiveWidx%
  winRequestRedraw%(crsrActiveWidx%) = 1
  IF winBuf%(0) = winBuf%(1) THEN 'If the other window looks into the same buffer, reset that one too.
    resetWindow NOT crsrActiveWidx%
    winRequestRedraw%(NOT crsrActiveWidx%) = 1
  ENDIF
  
  promptMsg "Buffer closed.", 1
END SUB

SUB crsrRightKeyHandler
  crsrRight 1
END SUB

SUB crsrRight num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    crsrRightImp
    ii%=ii%+1
  LOOP
END SUB

SUB crsrRightImp
  'First figure out the new buffer cursor position
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL oldBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)

  LOCAL lin$

  rdBufLine(bIdx%, oldBufCrsrRow%, lin$)

  IF oldBufCrsrCol% < LEN(lin$) THEN 'Move right on same line.
    winBufCrsrCol%(crsrActiveWidx%) = oldBufCrsrCol% + 1
    winBufCrsrTargetCol%(crsrActiveWidx%) = oldBufCrsrCol% + 1
  ELSEIF oldBufCrsrRow% < bufNumRows%(bIdx%)-1 THEN 'Go to position 0 on next line.
    winBufCrsrCol%(crsrActiveWidx%) = 0
    winBufCrsrTargetCol%(crsrActiveWidx%) = 0
    winBufCrsrRow%(crsrActiveWidx%) = oldBufCrsrRow% + 1
  ENDIF

  'Then figure out the new window cursor position
  LOCAL newBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL newBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL oldWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)
  LOCAL oldWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)

  IF newBufCrsrCol% > oldBufCrsrCol% THEN 'Try to move cursor right.
    IF oldWinCrsrCol% < winNumCols%(crsrActiveWidx%)-1 THEN 'We can move cursor right
      winWinCrsrCol%(crsrActiveWidx%) = oldWinCrsrCol% + 1
    ENDIF
  ELSEIF newBufCrsrRow% > oldBufCrsrRow% THEN 'Go to position 0 on next line
    winWinCrsrCol%(crsrActiveWidx%) = 0
    IF oldWinCrsrRow% < winNumRows%(crsrActiveWidx%)-1 THEN 'We can move cursor down
      winWinCrsrRow%(crsrActiveWidx%) = oldWinCrsrRow% + 1
    ENDIF
  ENDIF

  'Finally figure out if we should scroll.
  LOCAL newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)
  LOCAL newWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)

  IF (newBufCrsrRow% > oldBufCrsrRow%) AND (oldWinCrsrRow% = newWinCrsrRow%) THEN
    scrollVdelta crsrActiveWidx%, 1
  ENDIF
  IF (newBufCrsrCol% > oldBufCrsrCol%) AND (oldWinCrsrCol% = newWinCrsrCol%) THEN
    scrollHdelta crsrActiveWidx%, 1
  ELSEIF (newWinCrsrCol% < oldWinCrsrCol%) AND (oldBufCrsrCol% >= winNumCols%(crsrActiveWidx%)) THEN
    scrollHoffset crsrActiveWidx%, 0
  ENDIF

  IF selectMode%(crsrActiveWidx%) THEN
    crsrOff
    drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
    drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
  ENDIF
END SUB

SUB crsrLeft num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    crsrLeftImp
    ii%=ii%+1
  LOOP
END SUB

SUB crsrLeftImp
  LOCAL prevLin$
  LOCAL prevLinLen%

  'First figure out the new buffer cursor position
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL oldBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)

  IF oldBufCrsrCol% > 0 THEN 'Move left on same line.
    winBufCrsrCol%(crsrActiveWidx%) = oldBufCrsrCol% - 1
    winBufCrsrTargetCol%(crsrActiveWidx%) = oldBufCrsrCol% - 1
  ELSEIF oldBufCrsrRow% > 0 THEN 'Go to end position of previous line.
    rdBufLine(bIdx%, oldBufCrsrRow%-1 , prevLin$)
    prevLinLen% = LEN(prevLin$)
    winBufCrsrCol%(crsrActiveWidx%) = prevLinLen%
    winBufCrsrTargetCol%(crsrActiveWidx%) = prevLinLen%
    winBufCrsrRow%(crsrActiveWidx%) = oldBufCrsrRow% - 1
  ENDIF

  'Then figure out the new window cursor position
  LOCAL newBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL newBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL oldWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)
  LOCAL oldWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)

  IF newBufCrsrCol% < oldBufCrsrCol% THEN 'Try to move cursor left.
    IF oldWinCrsrCol% > 0 THEN 'We can move cursor left
      winWinCrsrCol%(crsrActiveWidx%) = oldWinCrsrCol% - 1
    ENDIF
  ELSEIF newBufCrsrRow% < oldBufCrsrRow% THEN 'Go to end on previous line
    winWinCrsrCol%(crsrActiveWidx%) = MIN(prevLinLen%, winNumCols%(crsrActiveWidx%)-1)
    IF oldWinCrsrRow% > 0 THEN 'We can move cursor up
      winWinCrsrRow%(crsrActiveWidx%) = oldWinCrsrRow% - 1
    ENDIF
  ENDIF

  'Finally figure out if we should scroll.
  LOCAL newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)
  LOCAL newWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)

  IF (newBufCrsrRow% < oldBufCrsrRow%) AND (oldWinCrsrRow% = newWinCrsrRow%) THEN
    scrollVdelta crsrActiveWidx%, -1
  ENDIF

  IF (newBufCrsrCol% < oldBufCrsrCol%) AND (oldWinCrsrCol% = newWinCrsrCol%) THEN
    scrollHdelta crsrActiveWidx%, -1
  ELSEIF (newWinCrsrCol% > oldWinCrsrCol%) AND (newBufCrsrCol% >= winNumCols%(crsrActiveWidx%)) THEN
    scrollHdelta crsrActiveWidx%, newBufCrsrCol% - (winNumCols%(crsrActiveWidx%) - 1)
  ENDIF

  IF selectMode%(crsrActiveWidx%) THEN
    crsrOff
    drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
    drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
  ENDIF
END SUB

SUB crsrLeftKeyHandler
  crsrLeft 1
END SUB

SUB crsrDown num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    crsrDownImp
    ii%=ii%+1
  LOOP
END SUB

SUB crsrDownImp
  'First figure out the new buffer cursor position
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL oldBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL nextLin$
  LOCAL nextLinLen%

  IF oldBufCrsrRow% < bufNumRows%(bIdx%)-1 THEN 'Move down.
    winBufCrsrRow%(crsrActiveWidx%) = oldBufCrsrRow% + 1
    rdBufLine(bIdx%, oldBufCrsrRow% + 1, nextLin$)
    nextLinLen% = LEN(nextLin$)
    winBufCrsrCol%(crsrActiveWidx%) = MIN(winBufCrsrTargetCol%(crsrActiveWidx%), nextLinLen%)
  ELSE
    endKeyHandler 1 'On the last row, jump to the last column.
    END SUB
  ENDIF

  'Then figure out the new window cursor position
  LOCAL newBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL newBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL oldWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)
  LOCAL oldWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)

  IF newBufCrsrRow% > oldBufCrsrRow% THEN 'Try to move cursor down.
    IF oldWinCrsrRow% < winNumRows%(crsrActiveWidx%)-1 THEN 'We can move cursor down
      winWinCrsrRow%(crsrActiveWidx%) = oldWinCrsrRow% + 1
    ENDIF
  ENDIF

  LOCAL newWinCrsrCol% = MIN(newBufCrsrCol%, winNumCols%(crsrActiveWidx%)-1)
  winWinCrsrCol%(crsrActiveWidx%) = newWinCrsrCol%

  'Finally figure out if we should scroll.
  LOCAL newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)

  IF (newBufCrsrRow% > oldBufCrsrRow%) AND (oldWinCrsrRow% = newWinCrsrRow%) THEN
    scrollVdelta crsrActiveWidx%, 1
  ENDIF

  scrollHoffset crsrActiveWidx%, newBufCrsrCol% - newWinCrsrCol%

  IF selectMode%(crsrActiveWidx%) THEN
    crsrOff
    drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
    drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
  ENDIF
END SUB

SUB crsrDownKeyHandler
  crsrDown 1
END SUB

SUB crsrUp num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    crsrUpImp
    ii%=ii%+1
  LOOP
END SUB

SUB crsrUpImp
  'First figure out the new buffer cursor position
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL oldBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)

  LOCAL prevLin$
  LOCAL prevLinLen%

  IF oldBufCrsrRow% > 0 THEN 'Move up.
    winBufCrsrRow%(crsrActiveWidx%) = oldBufCrsrRow% - 1
    rdBufLine(bIdx%, oldBufCrsrRow% - 1, prevLin$)
    prevLinLen% = LEN(prevLin$)
    winBufCrsrCol%(crsrActiveWidx%) = MIN(winBufCrsrTargetCol%(crsrActiveWidx%), prevLinLen%)
  ENDIF

  'Then figure out the new window cursor position
  LOCAL newBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL newBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL oldWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)
  LOCAL oldWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)

  IF newBufCrsrRow% < oldBufCrsrRow% THEN 'Try to move cursor up.
    IF oldWinCrsrRow% > 0 THEN 'We can move cursor up
      winWinCrsrRow%(crsrActiveWidx%) = oldWinCrsrRow% - 1
    ENDIF
  ENDIF

  winWinCrsrCol%(crsrActiveWidx%) = MIN(newBufCrsrCol%, winNumCols%(crsrActiveWidx%)-1)

  'Finally figure out if we should scroll.
  LOCAL newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)
  LOCAL newWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)

  IF (newBufCrsrRow% < oldBufCrsrRow%) AND (oldWinCrsrRow% = newWinCrsrRow%) THEN
    scrollVdelta crsrActiveWidx%, -1
  ENDIF 

  scrollHoffset crsrActiveWidx%, newBufCrsrCol% - newWinCrsrCol%

  IF selectMode%(crsrActiveWidx%) THEN
    crsrOff
    drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
    drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
  ENDIF
END SUB

SUB crsrUpKeyHandler
  crsrUp 1
END SUB

SUB home(nConsecHomeKeyPresses%)
  SELECT CASE nConsecHomeKeyPresses%
    CASE 1  'Go to beginning of line
      winBufCrsrCol%(crsrActiveWidx%) = 0
      winWinCrsrCol%(crsrActiveWidx%) = 0
      winBufCrsrTargetCol%(crsrActiveWidx%) = 0
      IF winBufTopCol%(crsrActiveWidx%) > 0 THEN
        scrollHoffset crsrActiveWidx%, 0
      ENDIF
    CASE 2 'Go to top of screen, beginning of line
      winBufCrsrRow%(crsrActiveWidx%) = winBufCrsrRow%(crsrActiveWidx%) - winWinCrsrRow%(crsrActiveWidx%)
      winWinCrsrRow%(crsrActiveWidx%) = 0
    CASE ELSE 'Go to top of buffer
      winBufCrsrRow%(crsrActiveWidx%) = 0
      IF winBufTopRow%(crsrActiveWidx%) > 0 THEN
        scrollVdelta crsrActiveWidx%, -winBufTopRow%(crsrActiveWidx%)
      ENDIF
  END SELECT

  IF selectMode%(crsrActiveWidx%) THEN
    winRequestRedraw%(crsrActiveWidx%) = 1
  ENDIF
END SUB

SUB homeKeyHandler(nConsecHomeKeyPresses%)
  home nConsecHomeKeyPresses%
END SUB

SUB endKeyHandler(nConsecEndKeyPresses%)
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL linLen%
  LOCAL oldBufCrsrRow%
  LOCAL newBufCrsrRow%
  LOCAL oldWinCrsrRow%
  LOCAL newWinCrsrRow%
  
  SELECT CASE nConsecEndKeyPresses%
    CASE 1  'Go to end of line
      rdBufLine(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin$)
      linLen% = LEN(lin$)
      winBufCrsrCol%(crsrActiveWidx%) = linLen%
      winWinCrsrCol%(crsrActiveWidx%) = MIN(linLen%, winNumCols%(crsrActiveWidx%)-1)
      winBufCrsrTargetCol%(crsrActiveWidx%) = linLen%
      scrollHoffset crsrActiveWidx%, MAX(0, linLen% - (winNumCols%(crsrActiveWidx%)-1))
    CASE 2 'Go to last row on screen, end of the line
      oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
      newBufCrsrRow% = oldBufCrsrRow% + (winNumRows%(crsrActiveWidx%) - 1 - winWinCrsrRow%(crsrActiveWidx%))
      newBufCrsrRow% = MIN(newBufCrsrRow%, bufNumRows%(bIdx%) - 1)
      winBufCrsrRow%(crsrActiveWidx%) = newBufCrsrRow%
      newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%) + (newBufCrsrRow% - oldBufCrsrRow%)
      winWinCrsrRow%(crsrActiveWidx%) = MIN(newWinCrsrRow%, winNumRows%(crsrActiveWidx%) - 1)
      rdBufLine(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin$)
      linLen% = LEN(lin$)
      winBufCrsrCol%(crsrActiveWidx%) = linLen%
      winWinCrsrCol%(crsrActiveWidx%) = MIN(linLen%, winNumCols%(crsrActiveWidx%)-1)
      winBufCrsrTargetCol%(crsrActiveWidx%) = linLen%
      scrollHoffset crsrActiveWidx%, MAX(0, linLen% - (winNumCols%(crsrActiveWidx%)-1))
    CASE ELSE 'Go to bottom of buffer
      oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
      newBufCrsrRow% = bufNumRows%(bIdx%) - 1
      winBufCrsrRow%(crsrActiveWidx%) = newBufCrsrRow%
      oldWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)
      newWinCrsrRow% = MIN(oldWinCrsrRow% + (newBufCrsrRow% - oldBufCrsrRow%), winNumRows%(crsrActiveWidx%) - 1)
      winWinCrsrRow%(crsrActiveWidx%) = newWinCrsrRow%

      IF (newBufCrsrRow% - oldBufCrsrRow%) - (newWinCrsrRow% - oldWinCrsrRow%) <> 0 THEN
        scrollVdelta crsrActiveWidx%, (newBufCrsrRow% - oldBufCrsrRow%) - (newWinCrsrRow% - oldWinCrsrRow%)
      ENDIF

      rdBufLine(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin$)
      linLen% = LEN(lin$)
      winBufCrsrCol%(crsrActiveWidx%) = linLen%
      winWinCrsrCol%(crsrActiveWidx%) = MIN(linLen%, winNumCols%(crsrActiveWidx%)-1)
      winBufCrsrTargetCol%(crsrActiveWidx%) = linLen%
      scrollHoffset crsrActiveWidx%, MAX(0, linLen% - (winNumCols%(crsrActiveWidx%)-1))
  END SELECT

  IF selectMode%(crsrActiveWidx%) THEN
    winRequestRedraw%(crsrActiveWidx%) = 1
  ENDIF
END SUB

SUB pgDownKeyHandler
  'First figure out the new buffer cursor position
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)

  'Do nothing if end is on the screen.
  IF winBufTopRow%(crsrActiveWidx%) + winNumRows%(crsrActiveWidx%) < bufNumRows%(bIdx%) THEN
    gotoBufPos MIN(oldBufCrsrRow% + winNumRows%(crsrActiveWidx%), bufNumRows%(bIdx%)), winBufCrsrTargetCol%(crsrActiveWidx%), 1, 0
  ENDIF
END SUB

SUB pgUpKeyHandler
  'First figure out the new buffer cursor position
  LOCAL oldBufCrsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  
  'Do nothing if top of buffer is on the screen
  IF winBufTopRow%(crsrActiveWidx%) > 0 THEN
    gotoBufPos MAX(oldBufCrsrRow% - winNumRows%(crsrActiveWidx%),0), winBufCrsrTargetCol%(crsrActiveWidx%), 1, 0
  ENDIF
END SUB

'Regular printable, non-enter keyhandler
SUB editKey key$
  LOCAL ok%
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  rdBufLine(bIdx%, crsrRow%, lin$)
  LOCAL linLen% = LEN(lin$)

  IF crsrMode% = CRSR_MODE_INS% THEN
    IF linLen% < 255 THEN 'There is room for one more
      lin$ = LEFT$(lin$, winBufCrsrCol%(crsrActiveWidx%)) + key$ + RIGHT$(lin$, linLen% - winBufCrsrCol%(crsrActiveWidx%))
      ok% = wrBufLine%(bIdx%, crsrRow%, lin$)
      crsrOff
      drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
      crsrRight 1
    ENDIF
  ELSE 'Overwrite mode:
    IF (linLen% = 255) AND (winBufCrsrCol%(crsrActiveWidx%) = 255) THEN
      EXIT SUB
    ENDIF
    lin$ = LEFT$(lin$, winBufCrsrCol%(crsrActiveWidx%)) + key$ + RIGHT$(lin$, MAX(0,linLen% - winBufCrsrCol%(crsrActiveWidx%) - 1))
    ok% = wrBufLine%(bIdx%, crsrRow%, lin$)
    crsrOff
    drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
    crsrRight 1
  ENDIF
END SUB

SUB undoEdit
  winSelectRow%(crsrActiveWidx%) = undoSelStartRow%(undoIdx%)
  winSelectCol%(crsrActiveWidx%) = undoSelStartCol%(undoIdx%)
  winBufCrsrRow%(crsrActiveWidx%) = undoSelEndRow%(undoIdx%)
  winBufCrsrCol%(crsrActiveWidx%) = undoSelEndCol%(undoIdx%)

  LOCAL ok%=deleteSelection%()
  IF undoBufNumRows%(undoIdx%) > 0 THEN
    undoDeleteSelection
  ENDIF
  gotoBufPos undoSelStartRow%(undoIdx%), undoSelStartCol%(undoIdx%), 0, 1 
END SUB

SUB regUndoEdit(undoRecordPending%)
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL crsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL undoRow%, bufLinLen%
  LOCAL bufLin$, undoBufLin$, char$

  IF NOT undoRecordPending% THEN
    undoSelStartRow%(undoIdx%) = crsrRow%
    undoSelStartCol%(undoIdx%) = crsrCol%
    IF crsrMode% = CRSR_MODE_OVR% THEN
      undoRow% = bufNumRows%(UNDO_BIDX%)
      undoBufLin$ = "" 'Start new record.
    ENDIF
  ELSE
    IF crsrMode% = CRSR_MODE_OVR% THEN
      undoRow% = undoBufStartRow%(undoIdx%)
      rdBufLine(UNDO_BIDX%, undoRow%, undoBufLin$) 'Add to record
    ENDIF
    
  ENDIF

  IF crsrMode% = CRSR_MODE_OVR% THEN
    rdBufLine(bIdx%, crsrRow%, bufLin$)
    bufLinLen% = LEN(bufLin$)

    IF crsrCol% < bufLinLen% THEN
      char$ = MID$(bufLin$, crsrCol% + 1, 1)
    ELSE
      char$ = ""
    ENDIF
    IF NOT wrBufLine%(UNDO_BIDX%, undoRow%, undoBufLin$+char$) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
  ENDIF

  undoAction%(undoIdx%) = UNDO_EDIT%
  undoSelEndRow%(undoIdx%) = crsrRow%
  undoSelEndCol%(undoIdx%) = crsrCol%+1
  IF crsrMode% = CRSR_MODE_OVR% THEN
    undoBufStartRow%(undoIdx%) = undoRow%
    undoBufNumRows%(undoIdx%) = 1
  ELSE
    undoBufStartRow%(undoIdx%) = -1
    undoBufNumRows%(undoIdx%) = 0
  ENDIF 
END SUB

SUB editKeyHandler key$
  IF selectMode%(crsrActiveWidx%) THEN
    promptMsg "Deleting selection...", 1

    registerForUndo UNDO_DELETE_SELECTION%
    IF NOT deleteSelection%() THEN
      undoRegisterForUndo
      EXIT SUB
    ENDIF

    promptMsg "Delete done.", 1
  ENDIF

  registerForUndo UNDO_EDIT%
  editKey key$
END SUB

FUNCTION numLeadingSpaces%(s$)
  LOCAL ii%=0
  FOR ii%=0 TO LEN(s$)-1
    IF MID$(s$, 1+ii%, 1) <> " " THEN
      numLeadingSpaces% = ii%
      EXIT FUNCTION
    ENDIF
  NEXT ii%
  numLeadingSpaces% = LEN(s$)
END FUNCTION

SUB enterKeyHandler
  IF selectMode%(crsrActiveWidx%) THEN
    promptMsg "Deleting selection...", 1

    registerForUndo UNDO_DELETE_SELECTION%
    IF NOT deleteSelection%() THEN
      undoRegisterForUndo
      EXIT SUB
    ENDIF

    promptMsg "Delete done.", 1 
  ENDIF

  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$, lin1$, lin2$
  LOCAL startRow%, startCol%
  LOCAL ok%
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)

  IF NOT insertBufLines%(bIdx%, crsrRow%+1, 1) THEN
    promptMsg "Buffer full. Can't insert line.", 1
    EXIT SUB
  ENDIF

  startRow% = winBufCrsrRow%(crsrActiveWidx%)
  startCol% = winBufCrsrCol%(crsrActiveWidx%)

  rdBufLine(bIdx%, crsrRow%, lin$)
  LOCAL nls% = MIN(numLeadingSpaces%(lin$), winBufCrsrCol%(crsrActiveWidx%))
  
  lin1$ = LEFT$(lin$, winBufCrsrCol%(crsrActiveWidx%))
  lin2$ = SPACE$(nls%) + MID$(lin$, 1+winBufCrsrCol%(crsrActiveWidx%))
  IF NOT wrBufLine%(bIdx%, crsrRow%+1, lin2$) THEN 'This one could fail (OOM), so do this first.
    promptMsg "Out of Memory. Can't insert line.", 1
    EXIT SUB
  ENDIF
  ok% = wrBufLine%(bIdx%, crsrRow%, lin1$)
  crsrDown 1
  home 1  
  crsrRight nls%
  
  'Note that typically we register for undo before applying the action but her
  'we do it after.
  winSelectCol%(crsrActiveWidx%) = startCol%
  winSelectRow%(crsrActiveWidx%) = startRow%
  registerForUndo UNDO_PASTE% 'Reusing the paster undo action.
  winSelectCol%(crsrActiveWidx%) = -1
  winSelectRow%(crsrActiveWidx%) = -1

  winRequestRedraw%(crsrActiveWidx%) = 1
  IF winBuf%(0) = winBuf%(1) THEN 'If other window looks into the same buffer, redraw that one too.
    winRequestRedraw%(NOT crsrActiveWidx%) = 1
  ENDIF 
END SUB

SUB regUndoBackspace(undoRecordPending%)
  LOCAL ok%
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL undoBufLin$, bufLin$, char$
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL crsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  rdBufLine(bIdx%, crsrRow%, bufLin$)
  LOCAL bufLinLen% = LEN(bufLin$)
  LOCAL undoRow%
  LOCAL numRows%

  IF undoRecordPending% THEN 'Add to record.
    numRows% = undoBufNumRows%(undoIdx%)
    undoRow% = undoBufStartRow%(undoIdx%) 'We add to the front, so stick to first row.
    rdBufLine(UNDO_BIDX%, undoRow%, undoBufLin$) 'Add to existing record
  ELSE
    numRows% = 1
    undoRow% = bufNumRows%(UNDO_BIDX%)
    undoBufLin$ = "" 'Start new record.
  ENDIF

  IF crsrCol% > 0 THEN
    char$ = MID$(bufLin$, crsrCol%, 1)
    IF NOT wrBufLine%(UNDO_BIDX%, undoRow%, char$+undoBufLin$) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    crsrCol% = crsrCol%-1
  ELSE
    crsrRow% = crsrRow%-1
    rdBufLine(bIdx%, crsrRow%, bufLin$)
    bufLinLen% = LEN(bufLin$)
    crsrCol% = bufLinLen%
    IF NOT insertBufLines%(UNDO_BIDX%, undoRow%, 1) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    IF NOT wrBufLine%(UNDO_BIDX%, undoRow%, "") THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    numRows% = numRows% + 1
  ENDIF

  undoAction%(undoIdx%) = UNDO_DELETE_SELECTION%
  undoSelStartRow%(undoIdx%) = crsrRow%
  undoSelStartCol%(undoIdx%) = crsrCol%
  undoSelEndRow%(undoIdx%) = 0
  undoSelEndCol%(undoIdx%) = 0
  undoBufStartRow%(undoIdx%) = undoRow%
  undoBufNumRows%(undoIdx%) = numRows%
END SUB

SUB backspace num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    backspaceImp
    ii%=ii%+1
  LOOP
END SUB

SUB backspaceImp
  IF winBufCrsrCol%(crsrActiveWidx%)=0 AND winBufCrsrRow%(crsrActiveWidx%)=0 THEN
    EXIT SUB
  ENDIF

  registerForUndo UNDO_BACKSPACE%
  crsrLeft 1
  IF NOT delete%() THEN
    crsrRight 1
    undoRegisterForUndo
  ENDIF
END SUB

SUB backspaceKeyHandler
  IF selectMode%(crsrActiveWidx%) THEN
    promptMsg "Deleting selection...", 1

    registerForUndo UNDO_DELETE_SELECTION%
    IF NOT deleteSelection%() THEN
      undoRegisterForUndo
      EXIT SUB
    ENDIF

    promptMsg "Deleting done.", 1 
  ELSE
    backspace 1
  ENDIF
END SUB

SUB undoDelete
  undoDeleteSelection
  winSelectCol%(crsrActiveWidx%) = winBufCrsrCol%(crsrActiveWidx%)
  winSelectRow%(crsrActiveWidx%) = winBufCrsrRow%(crsrActiveWidx%)
  gotoBufPos undoSelStartRow%(undoIdx%), undoSelStartCol%(undoIdx%), 0, 1
END SUB

SUB regUndoDelete(undoRecordPending%)
  LOCAL ok%
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL undoBufLin$, bufLin$, char$
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL crsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  rdBufLine(bIdx%, crsrRow%, bufLin$)
  LOCAL bufLinLen% = LEN(bufLin$)
  LOCAL undoRow%
  LOCAL numRows%

  IF undoRecordPending% THEN 'Add to record.
    numRows% = undoBufNumRows%(undoIdx%)
    undoRow% = undoBufStartRow%(undoIdx%) + numRows% - 1 'Stick to last row in use
    rdBufLine(UNDO_BIDX%, undoRow%, undoBufLin$) 'Add to record
  ELSE
    numRows% = 1
    undoRow% = bufNumRows%(UNDO_BIDX%)
    undoBufLin$ = "" 'Start new record.
  ENDIF

  IF crsrCol% < bufLinLen% THEN
    char$ = MID$(bufLin$, crsrCol% + 1, 1)
    IF NOT wrBufLine%(UNDO_BIDX%, undoRow%, undoBufLin$+char$) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
  ELSE
    IF NOT wrBufLine%(UNDO_BIDX%, undoRow%, undoBufLin$) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    IF NOT wrBufLine%(UNDO_BIDX%, undoRow%+1, "") THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    numRows% = numRows% + 1
  ENDIF

  undoAction%(undoIdx%) = UNDO_DELETE%
  undoSelStartRow%(undoIdx%) = crsrRow%
  undoSelStartCol%(undoIdx%) = crsrCol%
  undoSelEndRow%(undoIdx%) = 0
  undoSelEndCol%(undoIdx%) = 0
  IF NOT undoRecordPending% THEN
    undoBufStartRow%(undoIdx%) = undoRow%
  ENDIF
  undoBufNumRows%(undoIdx%) = numRows%
END SUB

FUNCTION delete%()
  LOCAL ok%
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  LOCAL crsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  rdBufLine(bIdx%, crsrRow%, lin$) 'Read line on which to operate.
  LOCAL linLen% = LEN(lin$)

  IF crsrCol% < linLen% THEN 'If not end of line, then just delete one character at crsr.
    'Annoying corner case for max length lines.
    IF crsrCol% <= 253 THEN 
      lin$ = LEFT$(lin$, crsrCol%) + MID$(lin$, crsrCol% + 2)
    ELSE
      lin$ = LEFT$(lin$, crsrCol%)
    ENDIF
    ok% = wrBufLine%(bIdx%, crsrRow%, lin$)
    crsrOff
    drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
  ELSE 'At the end of the line -> join next line (if there is one).
    IF crsrRow%+1 < bufNumRows%(bIdx%) THEN
      LOCAL lin1$
      rdBufLine(bIdx%, crsrRow%+1, lin1$)
      IF LEN(lin$)+LEN(lin1$) >= 255 THEN
        promptMsg "Can't delete. Would exceed max. line length",1
        delete% = 0
        EXIT FUNCTION
      ENDIF
      ok% = wrBufLine%(bIdx%, crsrRow%, lin$ + lin1$)
      deleteBufLines(bIdx%, crsrRow%+1, 1)
      winRequestRedraw%(crsrActiveWidx%) = 1
      IF winBuf%(0) = winBuf%(1) THEN 'If other window looks into the same buffer, redraw that one too.
        winRequestRedraw%(NOT crsrActiveWidx%) = 1
      ENDIF
    ENDIF
  ENDIF

  delete% = 1
END FUNCTION

SUB undoDeleteSelection
  LOCAL fromRow% = undoSelStartRow%(undoIdx%)
  LOCAL fromCol% = undoSelStartCol%(undoIdx%)
  LOCAL undoStartRow% = undoBufStartRow%(undoIdx%) 
  LOCAL numRows% = undoBufNumRows%(undoIdx%)

  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin1$, lin2$, tail$
  LOCAL ii%

  'This action is roughly equivalent to a paste action, but the text to paste
  'comes from the undo buffer. Implementation logic is derive from the pasteKeyHandler
  
  gotoBufPos fromRow%, fromCol%, 0, 1
  winSelectCol%(crsrActiveWidx%) = winBufCrsrCol%(crsrActiveWidx%)
  winSelectRow%(crsrActiveWidx%) = winBufCrsrRow%(crsrActiveWidx%)

  rdBufLine(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin1$)
  rdBufLine(UNDO_BIDX%, undoStartRow%, lin2$))
  tail$ = MID$(lin1$, 1+winBufCrsrCol%(crsrActiveWidx%))

  crsrOff
  IF numRows% = 1 THEN
    lin1$ = LEFT$(lin1$, winBufCrsrCol%(crsrActiveWidx%)) + lin2$ + tail$
    IF NOT wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin1$) THEN
      promptMsg "Out of Memory. Can't undo delete.", 1
      EXIT SUB
    ENDIF
  ELSE 'Multiple rows:
    lin1$ = LEFT$(lin1$, winBufCrsrCol%(crsrActiveWidx%)) + lin2$
    IF NOT wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin1$) THEN
      promptMsg "Out of Memory. Can't undo delete.", 1
      EXIT SUB
    ENDIF

    IF NOT insertBufLines%(bIdx%, winBufCrsrRow%(crsrActiveWidx%)+1, numRows%-1) THEN
      promptMsg "Buffer full. Can't undo delete.", 1
      EXIT SUB
    ENDIF
    crsrDown 1

    ii%=1
    DO WHILE ii% < numRows%-1
      rdBufLine(UNDO_BIDX%, undoStartRow%+ii%, lin2$)
      IF NOT wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin2$) THEN
        promptMsg "Out of Memory. Can't undo delete.", 1
        EXIT SUB
      ENDIF
      crsrDown 1
      ii% = ii%+1
    LOOP
    'Last row:
    rdBufLine(UNDO_BIDX%, undoStartRow%+numRows%-1, lin2$)
    IF NOT wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin2$+tail$) THEN
      promptMsg "Out of Memory. Can't undo delete.", 1
      EXIT SUB
    ENDIF

    home 1
  ENDIF

  crsrRight LEN(lin2$)

  IF numRows%=1 THEN
    drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
  ELSE
    winRequestRedraw%(crsrActiveWidx%) = 1
    IF winBuf%(0) = winBuf%(1) THEN 'If the other window looks into the same buffer, reset that one too.
      resetWindow NOT crsrActiveWidx%
      winRequestRedraw%(NOT crsrActiveWidx%) = 1
    ENDIF
  ENDIF
END SUB

'Register undo delete selection
SUB regUndoDeleteSelection
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL fromRow%, fromCol%, toRow%, toCol%, ok%, ii%
  LOCAL undoStartRow% = bufNumRows%(UNDO_BIDX%)

  selectionBoundaries(crsrActiveWidx%, fromRow%, fromCol%, toRow%, toCol%)

  'Copy selection to undo buffer
  
  rdBufLine(bIdx%, fromRow%, lin$)
  IF fromRow% = toRow% THEN
    IF NOT wrBufLine%(UNDO_BIDX%, undoStartRow%, MID$(lin$, fromCol%+1, toCol%-fromCol%)) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
  ELSE
    IF NOT wrBufLine%(UNDO_BIDX%, undoStartRow%, MID$(lin$, fromCol%+1)) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    ii% = fromRow%+1
    DO WHILE ii% < toRow%
      rdBufLine(bIdx%, ii%, lin$)
      IF NOT wrBufLine%(UNDO_BIDX%, undoStartRow% + ii%-fromRow%, lin$) THEN
        resetUndo 1
        EXIT SUB
      ENDIF
      ii% = ii%+1
    LOOP
    'Last line:
    rdBufLine(bIdx%, toRow%, lin$)
    IF NOT wrBufLine%(UNDO_BIDX%, undoStartRow% + toRow%-fromRow%, LEFT$(lin$, toCol%)) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
  ENDIF

  undoAction%(undoIdx%) = UNDO_DELETE_SELECTION%
  undoSelStartRow%(undoIdx%) = fromRow%
  undoSelStartCol%(undoIdx%) = fromCol%
  undoSelEndRow%(undoIdx%) = toRow%
  undoSelEndCol%(undoIdx%) = toCol%
  undoBufStartRow%(undoIdx%) = undoStartRow%
  undoBufNumRows%(undoIdx%) = toRow%-fromRow%+1
END SUB

FUNCTION deleteSelection%()
  LOCAL fromLin$, toLin$
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL fromRow%, fromCol%, toRow%, toCol%, ok%

  selectionBoundaries(crsrActiveWidx%, fromRow%, fromCol%, toRow%, toCol%)
  toCol% = toCol%-1

  LOCAL linLen%
  
  IF NOT (fromRow% = winBufCrsrRow%(crsrActiveWidx%) AND (fromCol% = winBufCrsrCol%(crsrActiveWidx%))) THEN
    'Go back to selection start point
    gotoBufPos fromRow%, fromCol%, 0, 1
  ENDIF

  rdBufLine(bIdx%, fromRow%, fromLin$)
  rdBufLine(bIdx%, toRow%, toLin$)
  linLen% = LEN(toLin$)

  LOCAL l$ = LEFT$(fromLin$, fromCol%)
  LOCAL r$ = RIGHT$(toLin$, linLen% - toCol% - 1)
  
  IF LEN(l$) + LEN(r$) > 255 THEN
    promptMsg "Can't delete selection. Would exceed max. line length.", 1
    deleteSelection% = 0
    EXIT FUNCTION
  ENDIF

  ok% = wrBufLine%(bIdx%, fromRow%, l$ + r$)
  deleteBufLines(bIdx%, fromRow%+1, toRow% - fromRow%)
  
  winRequestRedraw%(crsrActiveWidx%) = 1
  IF winBuf%(0) = winBuf%(1) THEN 'If other window looks into the same buffer, redraw that one too.
    winRequestRedraw%(NOT crsrActiveWidx%) = 1
  ENDIF

  deleteSelection% = 1
END FUNCTION

SUB deleteKeyHandler
  IF selectMode%(crsrActiveWidx%) THEN
    promptMsg "Deleting selection...", 1
    registerForUndo UNDO_DELETE_SELECTION%
    IF deleteSelection%() THEN
      promptMsg "Delete done.", 1
    ENDIF
  ELSE 'The actual work is done by delete% function. The rest here are corner case protections.
    LOCAL bIdx% = winBuf%(crsrActiveWidx%)
    LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
    LOCAL crsrCol% = winBufCrsrCol%(crsrActiveWidx%)
    LOCAL lin$
    rdBufLine(bIdx%, crsrRow%, lin$)
    LOCAL linLen% = LEN(lin$)
    IF (crsrCol% = linLen%) AND (crsrRow% = bufNumRows%(bIdx%)-1) THEN
      EXIT SUB
    ENDIF
    registerForUndo UNDO_DELETE%
    IF NOT delete%() THEN
      'If del failed, then remove the undo action for it.
      undoRegisterForUndo
    ENDIF
  ENDIF
END SUB

SUB undoIndentSelection
  winSelectRow%(crsrActiveWidx%) = undoSelStartRow%(undoIdx%)
  winSelectCol%(crsrActiveWidx%) = undoSelStartCol%(undoIdx%)
  gotoBufPos undoSelEndRow%(undoIdx%), undoSelEndCol%(undoIdx%), 0, 1
  unindentSelection
END SUB

'Register undo indent selection
SUB regUndoIndentSelection
  LOCAL startRow%, startCol%, endRow%, endCol%
  selectionBoundaries(crsrActiveWidx%, startRow%, startCol%, endRow%, endCol%)

  undoAction%(undoIdx%) = UNDO_INDENT%
  undoSelStartRow%(undoIdx%) = startRow%
  undoSelStartCol%(undoIdx%) = startCol%
  undoSelEndRow%(undoIdx%) = endRow%
  undoSelEndCol%(undoIdx%) = endCol%
  undoBufStartRow%(undoIdx%) = -1
  undoBufNumRows%(undoIdx%) = 0
END SUB

'A too complicated sub to snap a selection to the nearest next tab column.
SUB indentSelection
  LOCAL startRow%, startCol%, endRow%, endCol%
  LOCAL ii%, jj%
  LOCAL screenCtxt%(SCREEN_CONTEXT_SIZE%-1)
  saveScreenPos screenCtxt%()

  selectionBoundaries(crsrActiveWidx%, startRow%, startCol%, endRow%, endCol%)
  IF endCol%=0 THEN
    endRow% = endRow%-1
  ENDIF

  IF NOT (startRow% = winBufCrsrRow%(crsrActiveWidx%) AND (startCol% = winBufCrsrCol%(crsrActiveWidx%))) THEN
    'Go back to selection start point
    gotoBufPos startRow%, startCol%, 0, 1
  ENDIF
  
  LOCAL numSpaces% = TAB_WIDTH% - (winBufCrsrCol%(crsrActiveWidx%) MOD TAB_WIDTH%)
  winSelectCol%(crsrActiveWidx%) = winSelectCol%(crsrActiveWidx%) + numSpaces%

  jj% = startRow%
  DO WHILE jj% <= endRow%
    ii%=0
    home 1
    DO WHILE ii%<numSpaces%
      editKey " "
      ii% = ii%+1
    LOOP
    jj% = jj%+1
    crsrDown 1
  LOOP

  gotoBufPos startRow%, startCol%, 0, 1

  restoreScreenPos screenCtxt%()
  IF winBufCrsrCol%(crsrActiveWidx%) <> 0 THEN
    crsrRight numSpaces%
  ENDIF
END SUB

SUB indentKeyHandler
  LOCAL startRow%, startCol%, endRow%, endCol%

  selectionBoundaries(crsrActiveWidx%, startRow%, startCol%, endRow%, endCol%)

  IF selectMode%(crsrActiveWidx%) AND (endRow%<>startRow%) THEN
    registerForUndo UNDO_INDENT%
    indentSelection
  ELSE
    IF selectMode%(crsrActiveWidx%) THEN
      registerForUndo UNDO_DELETE_SELECTION%
      IF NOT deleteSelection%() THEN
        undoRegisterForUndo
        EXIT SUB
      ENDIF
    ENDIF

    'Insert spaces to nearest tab level
    LOCAL numSpaces% = TAB_WIDTH% - (winBufCrsrCol%(crsrActiveWidx%) MOD TAB_WIDTH%)
    LOCAL ii%=0

    DO WHILE ii%<numSpaces%
      registerForUndo UNDO_EDIT%
      editKey " "
      ii% = ii%+1
    LOOP
  ENDIF
END SUB

SUB undoUnindentSelection
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL fromRow% = undoSelStartRow%(undoIdx%)
  LOCAL fromCol% = undoSelStartCol%(undoIdx%)
  LOCAL toRow% = undoSelEndRow%(undoIdx%)
  LOCAL toCol% = undoSelEndCol%(undoIdx%)
  LOCAL undoStartRow% = undoBufStartRow%(undoIdx%)
  LOCAL lin$
  
  'We can't just reindent the selection as an undo action because lines may be bunched up
  'against the home column.

  'Copying full lines from undo buffer
  LOCAL ii% = 0
  DO WHILE ii% < toRow% - fromRow% + 1
    rdBufLine(UNDO_BIDX%, undoStartRow% + ii%, lin$)
    IF NOT wrBufLine%(bidx%, fromRow% + ii%, lin$) THEN
      promptMsg "Out of Memory. Can't undo delete.", 1
      EXIT SUB    
    ENDIF
    ii% = ii%+1
  LOOP

  winSelectRow%(crsrActiveWidx%) = undoSelStartRow%(undoIdx%)
  winSelectCol%(crsrActiveWidx%) = undoSelStartCol%(undoIdx%)

  gotoBufPos undoSelEndRow%(undoIdx%), undoSelEndCol%(undoIdx%), 0, 1
END SUB

'Register undo unindent selection
SUB regUndoUnindentSelection
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL fromRow%, fromCol%, toRow%, toCol%
  LOCAL undoStartRow% = bufNumRows%(UNDO_BIDX%)
  
  selectionBoundaries(crsrActiveWidx%, fromRow%, fromCol%, toRow%, toCol%)

  'We can't just reindent the selection as an undo action because lines may be bunched up
  'against the home column.
  
  'Copying full lines to undo buffer
  LOCAL ii% = fromRow%
  DO WHILE ii% <= toRow%
    rdBufLine(bIdx%, ii%, lin$)
    IF NOT wrBufLine%(UNDO_BIDX%, undoStartRow% + ii%-fromRow%, lin$) THEN
      resetUndo 1
      EXIT SUB
    ENDIF
    ii% = ii%+1
  LOOP

  undoAction%(undoIdx%) = UNDO_UNINDENT%
  undoSelStartRow%(undoIdx%) = fromRow%
  undoSelStartCol%(undoIdx%) = fromCol%
  undoSelEndRow%(undoIdx%) = toRow%
  undoSelEndCol%(undoIdx%) = toCol%
  undoBufStartRow%(undoIdx%) = undoStartRow%
  undoBufNumRows%(undoIdx%) = toRow%-fromRow%+1
END SUB

'Move selection to nearest previous tab column.
SUB unindentSelection
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL startRow%, startCol%, endRow%, endCol%
  LOCAL ii%, jj%, ok%
  LOCAL screenCtxt%(SCREEN_CONTEXT_SIZE%-1)
  LOCAL lin$
  saveScreenPos screenCtxt%()

  selectionBoundaries(crsrActiveWidx%, startRow%, startCol%, endRow%, endCol%)
  IF endCol%=0 THEN
    endRow% = endRow%-1
  ENDIF

  IF NOT (startRow% = winBufCrsrRow%(crsrActiveWidx%) AND (startCol% = winBufCrsrCol%(crsrActiveWidx%))) THEN
    'Go back to selection start point
    gotoBufPos startRow%, startCol%, 0, 1
  ENDIF
  
  'Number of space to move to previous tab level.
  LOCAL numSpaces% = (winBufCrsrCol%(crsrActiveWidx%) MOD TAB_WIDTH%)
  IF numSpaces% = 0 THEN
    numSpaces% = TAB_WIDTH%
  ENDIF
  winSelectCol%(crsrActiveWidx%) = MAX(0, winSelectCol%(crsrActiveWidx%) - numSpaces%)

  jj% = startRow%
  DO WHILE jj% <= endRow%
    ii% = 0
    home 1
    rdBufLine(bIdx%, jj%, lin$)
    DO WHILE ii% < numSpaces%
      IF (LEN(lin$) > ii%) AND MID$(lin$, 1+ii%, 1) = " " THEN
        ok% = delete%()
      ENDIF
      ii% = ii% + 1
    LOOP
    jj% = jj%+1
    crsrDown 1
  LOOP

  gotoBufPos startRow%, startCol%, 0, 1

  restoreScreenPos screenCtxt%()
  IF winBufCrsrCol%(crsrActiveWidx%) <> 0 THEN
    ii%=0
    DO WHILE ii%<numSpaces%
      IF winBufCrsrCol%(crsrActiveWidx%) > 0 THEN
        crsrLeft 1
      ENDIF
      ii% = ii%+1
    LOOP
  ENDIF
END SUB

SUB unindent
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL crsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL numSpacesToMove%
  LOCAL crsrRow% = winBufCrsrRow%(crsrActiveWidx%)
  rdBufLine(bIdx%, crsrRow%, lin$)
  LOCAL linLen% = LEN(lin$)
  LOCAL ii%
  LOCAL posFound% = 0

  'Find first non-space character to our right
  DO WHILE (NOT posFound%) AND (crsrCol% < linLen%)
    'Note: MID starts at 1.
    IF MID$(lin$, 1+crsrCol%, 1) = " " THEN
      crsrCol% = crsrCol% + 1
    ELSE
      posFound% = 1
    ENDIF
  LOOP

  IF NOT posFound% THEN
    EXIT SUB
  ENDIF

  'Number of space to move to previous tab level.
  numSpacesToMove% = (crsrCol% MOD TAB_WIDTH%)
  IF numSpacesToMove% = 0 THEN
    numSpacesToMove% = TAB_WIDTH%
  ENDIF

  IF crsrCol% < numSpacesToMove% THEN
    EXIT SUB
  ENDIF

  crsrRight crsrCol% - winBufCrsrCol%(crsrActiveWidx%)
  
  'Move back to previous tab level, if we can
  'Are there all spaces in before us?
  IF MID$(lin$, 1+crsrCol%-numSpacesToMove%, numSpacesToMove%) = SPACE$(numSpacesToMove%) THEN
    backspace numSpacesToMove%
  ENDIF
END SUB

SUB unindentKeyHandler
  IF selectMode%(crsrActiveWidx%) THEN
    registerForUndo UNDO_UNINDENT%
    unindentSelection
  ELSE
    unindent
  ENDIF
END SUB

SUB cutKeyHandler
  promptMsg "Cutting...", 1
  copyAction
  registerForUndo UNDO_DELETE_SELECTION%
  IF NOT deleteSelection%() THEN
    undoRegisterForUndo
    EXIT SUB
  ENDIF

  promptMsg "Cut done.", 1
END SUB

'Emergency reset of the clipboard.
SUB resetClipBoard
  LOCAL ii% = 0

  LOCAL ok$ = promptForAnyKey$("Out of memory. Clipboard will be cleared. Press any key to continue.")
  'Clear out the clipboard
  DO WHILE ii% < bufNumRows%(CLIPBOARD_BIDX%)
    freeLine bufLinePtrs%(ii%, CLIPBOARD_BIDX%)
    ii% = ii%+1
  LOOP
  bufNumRows%(CLIPBOARD_BIDX%) = 0
END SUB

'Copy to clipboard action
SUB copyAction
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin$
  LOCAL fromRow%, fromCol%, toRow%, toCol%, ok%, ii%=0

  IF NOT selectMode%(crsrActiveWidx%) THEN
    EXIT SUB
  ENDIF

  selectionBoundaries(crsrActiveWidx%, fromRow%, fromCol%, toRow%, toCol%)

  'Clear out the clipboard
  DO WHILE ii% < bufNumRows%(CLIPBOARD_BIDX%)
    freeLine bufLinePtrs%(ii%, CLIPBOARD_BIDX%)
    ii% = ii%+1
  LOOP
  bufNumRows%(CLIPBOARD_BIDX%) = 0

  rdBufLine(bIdx%, fromRow%, lin$)
  IF fromRow% = toRow% THEN
    IF NOT wrBufLine%(CLIPBOARD_BIDX%, 0, MID$(lin$, fromCol%+1, toCol%-fromCol%)) THEN
      resetClipBoard
      EXIT SUB
    ENDIF
  ELSE
    IF NOT wrBufLine%(CLIPBOARD_BIDX%, 0, MID$(lin$, fromCol%+1)) THEN
      resetClipboard
      EXIT SUB
    ENDIF
    ii% = fromRow%+1
    DO WHILE ii% < toRow%
      rdBufLine(bIdx%, ii%, lin$)
      IF NOT wrBufLine%(CLIPBOARD_BIDX%, ii%-fromRow%, lin$) THEN
        resetClipboard
        EXIT SUB
      ENDIF
      ii% = ii%+1
    LOOP
    'Last line:
    rdBufLine(bIdx%, toRow%, lin$)
    IF NOT wrBufLine%(CLIPBOARD_BIDX%, toRow%-fromRow%, LEFT$(lin$, toCol%)) THEN
      resetClipboard
      EXIT SUB
    ENDIF
  ENDIF
END SUB

SUB copyKeyHandler
  promptMsg "Copying...", 1
  copyAction
  promptMsg "Copy done.", 1
END SUB

SUB regUndoPaste
  undoAction%(undoIdx%) = UNDO_EDIT%
  undoSelStartRow%(undoIdx%) = winSelectRow%(crsrActiveWidx%)
  undoSelStartCol%(undoIdx%) = winSelectCol%(crsrActiveWidx%)
  undoSelEndRow%(undoIdx%) = winBufCrsrRow%(crsrActiveWidx%)
  undoSelEndCol%(undoIdx%) = winBufCrsrCol%(crsrActiveWidx%)
  undoBufStartRow%(undoIdx%) = -1
  undoBufNumRows%(undoIdx%) = 0
END SUB

'Paste from clipboard
SUB pasteKeyHandler
  LOCAL numRows% = bufNumRows%(CLIPBOARD_BIDX%)
  LOCAL startRow%, startCol%
  IF numRows% = 0 THEN 'Nothing to paste.
    EXIT SUB
  ENDIF

  promptMsg "Pasting...", 1
  IF selectMode%(crsrActiveWidx%) THEN
    registerForUndo UNDO_DELETE_SELECTION%
    IF NOT deleteSelection%() THEN
      undoRegisterForUndo
      EXIT SUB
    ENDIF
  ENDIF

  startRow% = winBufCrsrRow%(crsrActiveWidx%)
  startCol% = winBufCrsrCol%(crsrActiveWidx%)

  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL lin1$, lin2$, tail$
  LOCAL ii%, ok%

  rdBufLine(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin1$)
  rdBufLine(CLIPBOARD_BIDX%, 0, lin2$))
  tail$ = MID$(lin1$, 1+winBufCrsrCol%(crsrActiveWidx%))

  crsrOff

  LOCAL lft$ = LEFT$(lin1$, winBufCrsrCol%(crsrActiveWidx%))
  IF numRows% = 1 THEN
    IF LEN(lft$) + LEN(lin2$) + LEN(tail$) > 255 THEN
      promptMsg "Can't paste. Max. line length would be exceeded.", 1
      EXIT SUB
    ENDIF
    lin1$ = lft$ + lin2$ + tail$
    ok% = wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin1$)
  ELSE 'Multiple rows:
    IF LEN(lft$) + LEN(lin2$) > 255 THEN
      promptMsg "Can't paste. Max. line length would be exceeded.", 1
      EXIT SUB
    ENDIF

    lin1$ = lft$ + lin2$
    ok% = wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin1$)

    IF NOT insertBufLines%(bIdx%, winBufCrsrRow%(crsrActiveWidx%)+1, numRows%-1) THEN
      promptMsg "Out of Memory. Paste action aborted.", 1
      EXIT SUB
    ENDIF
    crsrDown 1

    ii%=1
    DO WHILE ii% < numRows%-1
      rdBufLine(CLIPBOARD_BIDX%, ii%, lin2$)
      IF NOT wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin2$) THEN
        promptMsg "Out of Memory. Paste action aborted.", 1
        EXIT SUB
      ENDIF
      crsrDown 1
      ii% = ii%+1
    LOOP
    'Last row:
    rdBufLine(CLIPBOARD_BIDX%, numRows%-1, lin2$)

    IF LEN(lin2$) + LEN(tail$) > 255 THEN
      promptMsg "Can't paste. Max. line length would be exceeded.", 1
      EXIT SUB
    ENDIF

    IF NOT wrBufLine%(bIdx%, winBufCrsrRow%(crsrActiveWidx%), lin2$+tail$) THEN
      promptMsg "Out of Memory. Paste action aborted.", 1
      EXIT SUB
    ENDIF

    home 1
  ENDIF

  crsrRight LEN(lin2$) 

  'Note that typically we register for undo before applying the action but her
  'we do it after.
  winSelectCol%(crsrActiveWidx%) = startCol%
  winSelectRow%(crsrActiveWidx%) = startRow%
  registerForUndo UNDO_PASTE%
  winSelectCol%(crsrActiveWidx%) = -1
  winSelectRow%(crsrActiveWidx%) = -1

  IF numRows%=1 THEN
    drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
  ELSE
    winRequestRedraw%(crsrActiveWidx%) = 1
    IF winBuf%(0) = winBuf%(1) THEN 
      winRequestRedraw%(NOT crsrActiveWidx%) = 1
    ENDIF
  ENDIF

  promptMsg "Paste done.", 1
END SUB

'Sticky cursor = 1 avoids moving the cursor and scrolls the page instead.
'Sticky cursor = 0 avoids scrolling the page and moves the cursor instead.
SUB gotoBufPos(bufRow%, bufCol%, stickyCursor%, resetCrsrTargetCol%)
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL oldBufCrsrCol% = winBufCrsrCol%(crsrActiveWidx%)
  LOCAL oldWinCrsrCol% = winWinCrsrCol%(crsrActiveWidx%)
  LOCAL newWinCrsrCol%
  LOCAL newBufCrsrCol%
  LOCAL lin$

  IF stickyCursor% THEN
    'Figure out new topRow, avoiding win cursor movement.
    winBufTopRow%(crsrActiveWidx%) = MAX(bufRow% - winWinCrsrRow%(crsrActiveWidx%), 0)
  ELSE
    'Scroll entire pages only.

    LOCAL numPagesToScroll% = (bufRow% - winBufTopRow%(crsrActiveWidx%))\winNumRows%(crsrActiveWidx%)

    IF bufRow% < winBufTopRow%(crsrActiveWidx%) THEN
      numPagesToScroll% = numPagesToScroll% - 1
    ENDIF

    winBufTopRow%(crsrActiveWidx%) = MAX(winBufTopRow%(crsrActiveWidx%) + numPagesToScroll%*winNumRows%(crsrActiveWidx%), 0)
  ENDIF

  'Figure out new win cursor given new top row
  winWinCrsrRow%(crsrActiveWidx%) = bufRow% - winBufTopRow%(crsrActiveWidx%)

  'Update buffer row cursor
  winBufCrsrRow%(crsrActiveWidx%) = bufRow%

  'Column update...
  rdBufLine(bIdx%, bufRow%, lin$)
  newBufCrsrCol% = MIN(bufCol%, LEN(lin$))
  winBufCrsrCol%(crsrActiveWidx%) = newBufCrsrCol%
  IF resetCrsrTargetCol% THEN
    winBufCrsrTargetCol%(crsrActiveWidx%) = newBufCrsrCol%
  ENDIF
  newWinCrsrCol% = MIN(newBufCrsrCol%, winNumCols%(crsrActiveWidx%)-1)
  winWinCrsrCol%(crsrActiveWidx%) = newWinCrsrCol%
  scrollHoffset crsrActiveWidx%, newBufCrsrCol% - newWinCrsrCol%

  winRequestRedraw%(crsrActiveWidx%) = 1
END SUB

SUB gotoKeyHandler
  LOCAL ans$ = promptForText$("Go To Line: ")
  LOCAL gotoLine% = VAL(ans$)

  IF gotoLine% = 0 THEN
    promptMsg "Invalid line number.", 1
    EXIT SUB
  ENDIF

  'User provided row numbers are base 1.
  gotoBufPos gotoLine% - 1, winBufCrsrTargetCol%(crsrActiveWidx%), 0, 1 
END SUB

SUB findNextKeyHandler
  IF strToFind$="" THEN
    findKeyHandler
  ELSE
    findStrToFind 'Keep searching for the same string.
  ENDIF
END SUB

SUB findKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL selStartRow%, selStartCol%, selEndRow%, selEndCol%

  strToFind$=""
  
  'If there's a selection, use that as search text. Otherwise prompt for text to search.
  IF selectMode%(crsrActiveWidx%) THEN
    selectionBoundaries(crsrActiveWidx%, selStartRow%, selStartCol%, selEndRow%, selEndCol%)

    IF selStartRow% = selEndRow% THEN
      rdBufLine(bIdx%, selStartRow%, strToFind$)
      strToFind$ = MID$(strToFind$, 1+selStartCol%, selEndCol%-selStartCol%)
    ENDIF
  ENDIF

  IF strToFind$ = "" THEN
    strToFind$ = promptForText$("Find String: ")
  ENDIF

  IF strToFind$ = "" THEN
    EXIT SUB
  ENDIF

  findStrToFind
END SUB
  
SUB findStrToFind
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL row%, col%, linLen%
  LOCAL lin$
  LOCAL key$
  LOCAL direction% = 1
  LOCAL strToFindAbbr$
  CONST STR_TO_FIND_ABBR_MAX% = 40
  
  IF LEN(strToFind$) > STR_TO_FIND_ABBR_MAX%) THEN
    strToFindAbbr$ = CHR$(34)+LEFT$(strToFind, STR_TO_FIND_ABBR_MAX%)+"..."+CHR$(34)
  ELSE
    strToFindAbbr$ = CHR$(34)+strToFind$+CHR$(34)
  ENDIF

  promptMsg "Searching for "+strToFindAbbr$+"...", 1

  IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
    strToFind$=UCASE$(strToFind$)
  ENDIF
  
  row%=winBufCrsrRow%(crsrActiveWidx%)
  col%=winBufCrsrCol%(crsrActiveWidx%)
  DO WHILE 1
    DO WHILE (row%>=0) AND (row% < bufNumRows%(bIdx%))
      rdBufLine(bIdx%, row%, lin$)
      linLen% = LEN(lin$)
      
      IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
        lin$=UCASE$(lin$)
      ENDIF
      
      IF direction% = 1 THEN
        col% = 0
      ELSE
        col% = linLen%-1
      ENDIF

      'There may be multiple hits on the same row, so we have to loop over columns
      'until the end of the line is reached.
      DO WHILE (col%>=0) AND (col% < linLen%)
        IF INSTR(1+col%, lin$, strToFind$) = 1+col% THEN
          gotoBufPos row%, col%, 0, 1

          winSelectCol%(crsrActiveWidx%) = col% + LEN(strToFind$)
          winSelectRow%(crsrActiveWidx%) = row%
          winBufCrsrCol%(crsrActiveWidx%) = col% 
          winBufCrsrRow%(crsrActiveWidx%) = row%

          'We can't just flag a request for redraw here because we're not returning
          'to the mainloop (yet). We block at the prompt.
          drawWinContents crsrActiveWidx%
          drawWinHeader crsrActiveWidx%

          key$ = UCASE$(promptForAnyKey$("Find "+strToFindAbbr$+", N=Next/P=Previous/Enter=Done."))
          SELECT CASE key$
            CASE "N"
              direction% = 1
            CASE "P"
              direction% = -1
            CASE ELSE
              EXIT SUB
          END SELECT

          promptMsg "Searching for "+strToFindAbbr$+"...", 1
        ENDIF
        col% = col% + direction%
      LOOP
      
      row% = row% + direction%
    LOOP

    IF row%<0 THEN
      IF UCASE$(promptForAnyKey$("Beginning of buffer reached. Wrap around? (Y/N)")) <> "Y" THEN
        EXIT SUB
      ENDIF
      row% = bufNumRows%(bIdx%)-1
      endKeyHandler 3
    ELSE
      IF UCASE$(promptForAnyKey$("End of buffer reached. Wrap around? (Y/N)")) <> "Y" THEN
        EXIT SUB
      ENDIF
      row% = 0
      home 3
    ENDIF
    
    promptMsg "Searching for "+strToFindAbbr$+"...", 1
  LOOP
END SUB

SUB replaceKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL strToReplace$ = "", replaceWith$ = ""
  LOCAL strToReplaceAbbr$
  CONST STR_TO_REPLACE_ABBR_MAX% = 40

  IF selectMode%(crsrActiveWidx%) THEN
    LOCAL selStartRow%, selStartCol%, selEndRow%, selEndCol%

    selectionBoundaries(crsrActiveWidx%, selStartRow%, selStartCol%, selEndRow%, selEndCol%)

    IF selStartRow% = selEndRow% THEN
      rdBufLine(bIdx%, selStartRow%, strToReplace$)
      strToReplace$ = MID$(strToReplace$, 1+selStartCol%, selEndCol%-selStartCol%)
    ENDIF
  ENDIF

  IF strToReplace$ = "" THEN
    strToReplace$ = promptForText$("Find String: ")
  ENDIF

  IF strToReplace$ = "" THEN
    EXIT SUB
  ENDIF

  IF LEN(strToReplace$) > STR_TO_REPLACE_ABBR_MAX%) THEN
    strToReplaceAbbr$ = CHR$(34)+LEFT$(strToReplace, STR_TO_REPLACE_ABBR_MAX%)+"..."+CHR$(34)
  ELSE
    strToReplaceAbbr$ = CHR$(34)+strToReplace$+CHR$(34)
  ENDIF

  replaceWith$ = promptForText$("Replace with: ")
  IF replaceWith$ = "" THEN
    EXIT SUB
  ENDIF

  IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
    strToReplace$=UCASE$(strToReplace$)
  ENDIF

  LOCAL row%, col%, startRow%, startCol%, linLen%, ok%
  LOCAL lin$, l$, r$
  LOCAL key$=""
  LOCAL wrappedAround% = 0

  row%=winBufCrsrRow%(crsrActiveWidx%)
  col%=winBufCrsrCol%(crsrActiveWidx%)
  startRow% = winBufCrsrRow%(crsrActiveWidx%)
  startCol% = winBufCrsrCol%(crsrActiveWidx%)

  promptMsg "Searching for "+strToReplaceAbbr$+"...", 1

  DO WHILE 1
    DO WHILE (row%>=0) AND (row% < bufNumRows%(bIdx%))
      rdBufLine(bIdx%, row%, lin$)
      linLen% = LEN(lin$)

      IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
        lin$=UCASE$(lin$)
      ENDIF

      col% = 0
      
      'There may be multiple hits on the same row, so we have to loop over columns
      'until the end of the line is reached.
      DO WHILE (col%>=0) AND (col% < linLen%)
        'key$="A" when a replace-all is requested.
        'This check tests if replace-all has gone full circle
        'The y/n cases don't use this. They rely on the end/beginning of buffer
        'reached prompts.
        IF (key$="A") AND (row%>=startRow%) AND wrappedAround% THEN
          gotoBufPos startRow%, startCol%, 0, 1
          promptMsg "Replace done.", 1
          EXIT SUB
        ENDIF

        IF INSTR(1+col%, lin$, strToReplace$) = 1+col% THEN
          gotoBufPos row%, col%, 0, 1

          winSelectCol%(crsrActiveWidx%) = col% + LEN(strToReplace$)
          winSelectRow%(crsrActiveWidx%) = row%
          winBufCrsrCol%(crsrActiveWidx%) = col% 
          winBufCrsrRow%(crsrActiveWidx%) = row%

          'We can't just flag a request for redraw here because we're not returning
          'to the mainloop (yet). We block at the prompt.
          drawWinContents crsrActiveWidx%
          drawWinHeader crsrActiveWidx%

          IF key$<>"A" THEN
            key$ = UCASE$(promptForAnyKey$("Replace? (Y)es/(N)o/(A)ll/Enter=Done."))
          ENDIF

          SELECT CASE key$
            CASE "N"
            CASE "Y","A"
              l$ = LEFT$(lin$, col%)
              r$ = MID$(lin$, 1+col% + LEN(strToReplace$))
              IF LEN(l$)+LEN(replaceWith$)+LEN(r$) > 255 THEN
                promptMsg "Can't replace. Would exceed max. line length.", 1
                EXIT SUB
              ENDIF
              lin$ = l$ + replaceWith$ + r$
              ok% = wrBufLine%(bIdx%, row%, lin$)
              crsrOff
              drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
              col% = col% + MAX(LEN(replaceWith$)-1,0)
            CASE ELSE
              EXIT SUB
          END SELECT
          
          promptMsg "Searching for "+strToReplaceAbbr$+"...", 1
        ENDIF
        col% = col% + 1
      LOOP
      
      row% = row% + 1
    LOOP

    IF key$ <> "A" THEN
      IF UCASE$(promptForAnyKey$("END of buffer reached. Wrap around? (Y/N)")) <> "Y" THEN
        EXIT SUB
      ENDIF
      promptMsg "Searching for "+strToReplaceAbbr$+"...", 1
      home 3
    ENDIF
    wrappedAround% = 1
    row% = 0
  LOOP
END SUB

'This is just a dispatcher.
SUB undoKeyHandler
  promptMsg "Undoing...", 1
  SELECT CASE undoAction%(undoIdx%)
    CASE UNDO_DELETE_SELECTION%
      undoDeleteSelection
    CASE UNDO_DELETE%
      undoDelete
    CASE UNDO_EDIT%
      undoEdit
    CASE UNDO_INDENT%
      undoIndentSelection
    CASE UNDO_UNINDENT%
      undoUnindentSelection
    CASE ELSE
      promptMsg "Nothing to undo.", 1
      EXIT SUB 
  END SELECT

  'Clear out this undo action and advance.
  clrUndoEntry
  undoIdx% = undoIdx% - 1
  IF undoIdx% < 0 THEN
    undoIdx% = MAX_NUM_UNDOS% - 1
  ENDIF

  winSelectRow%(crsrActiveWidx%) = -1
  winSelectCol%(crsrActiveWidx%) = -1

  promptMsg "Undone.", 1
END SUB

'Each register undo operation that has associated text stores it in the undo
'buffer, each time moving on into the buffer. So after a while we'll be well
'advanced into the undo buffer while the lines at the beginning of the buffer
'are most likely already released (there're max 16 undos tracked). When we're
'about halfway into the buffer this sub will be called which will recompacted
'everything so we should be able to keep going without ever reaching the end
'of the undo buffer.
SUB compactUndoBuffer
  LOCAL ii%=0, jj%
  LOCAL emptyBlockStart%=-1
  LOCAL emptyBlockEnd%=-1

  promptMsg "Compacting Undo buffer. Please wait...", 1

  DO WHILE ii% < bufNumRows%(UNDO_BIDX)
    IF bufLinePtrs%(ii%, UNDO_BIDX%) = -1 THEN
      IF emptyBlockStart%=-1 THEN
        emptyBlockStart% = ii%
        emptyBlockEnd% = ii%
      ELSE
        emptyBlockEnd% = ii%
      ENDIF
    ELSE
      'Did we find an empty block?
      IF emptyBlockStart% <> -1 THEN
        deleteBufLines(UNDO_BIDX%, emptyBlockStart%, emptyBlockEnd%+1-emptyBlockStart%)
        'Also adjust any undoBufStartRows that may be affected
        FOR jj%=0 TO MAX_NUM_UNDOS%-1
          IF undoBufStartRow%(jj%) > emptyBlockEnd% THEN
            undoBufStartRow%(jj%) = undoBufStartRow%(jj%) - (emptyBlockEnd%+1-emptyBlockStart%)
          ENDIF
        NEXT jj%
      ENDIF
    ENDIF

    ii% = ii%+1
  LOOP

  promptMsg "Undo buffer compacted.", 1
END SUB

'Clear out previous undo action at current location
SUB clrUndoEntry
  LOCAL ii%=0
  DO WHILE ii% < undoBufNumRows%(undoIdx%)
    freeLine bufLinePtrs%(undoBufStartRow%(undoIdx%)+ii%, UNDO_BIDX%)
    ii% = ii%+1
  LOOP
  undoBufStartRow%(undoIdx%) = -1
  undoBufNumRows%(undoIdx%) = 0
  undoAction%(undoIdx%) = 0
END SUB

SUB newUndoEntry
  undoIdx% = undoIdx% + 1
  IF undoIdx% >= MAX_NUM_UNDOS% THEN
    undoIdx% = 0
  ENDIF

  clrUndoEntry
END SUB

SUB undoRegisterForUndo
  'Clear out this undo action and advance.
  clrUndoEntry
  undoIdx% = undoIdx% - 1
  IF undoIdx% < 0 THEN
    undoIdx% = MAX_NUM_UNDOS% - 1
  ENDIF
END SUB

'A dispatcher for register for undo actions, with support for adding onto an
'existing undo record (e.g. when typing text, or when pressing delete mulitple
'times in a row.
SUB registerForUndo(action%)
  STATIC lastDel% = -1
  STATIC lastBckSpc% = -1
  STATIC lastEdit% = -1
  LOCAL undoRecordPending%

  SELECT CASE action%
    CASE UNDO_DELETE_SELECTION%
      newUndoEntry
      regUndoDeleteSelection
    CASE UNDO_ENTER%
      newUndoEntry
      regUndoEnter
    CASE UNDO_DELETE%
      'If last key press also trigered an undo delete, we add to the record.
      IF keyCounter% <= lastDel% + 1 THEN
        undoRecordPending% = 1
      ELSE
        undoRecordPending% = 0
        newUndoEntry 
      ENDIF
      lastDel% = keyCounter%
      regUndoDelete(undoRecordPending%)
    CASE UNDO_BACKSPACE%
      'If last key press also trigered an undo backspace, we add to the record.
      IF keyCounter% <= lastBckSpc% + 1 THEN
        undoRecordPending% = 1
      ELSE
        undoRecordPending% = 0
        newUndoEntry 
      ENDIF
      lastBckSpc% = keyCounter%
      regUndoBackspace(undoRecordPending%)
    CASE UNDO_EDIT%
      'If last key press also trigered an undo edit, we add to the record.
      IF keyCounter% <= lastEdit% + 1 THEN
        undoRecordPending% = 1
      ELSE
        undoRecordPending% = 0
        newUndoEntry 
      ENDIF
      lastEdit% = keyCounter%
      regUndoEdit(undoRecordPending%)
    CASE UNDO_PASTE%
      newUndoEntry
      regUndoPaste
    CASE UNDO_INDENT%
      newUndoEntry
      regUndoIndentSelection
    CASE UNDO_UNINDENT%
      newUndoEntry
      regUndoUnindentSelection
  END SELECT

  IF bufNumRows%(UNDO_BIDX%) > (MAX_NUM_ROWS%\2) THEN
    compactUndoBuffer
    'Check again, after compacting
    IF bufNumRows%(UNDO_BIDX%) > (MAX_NUM_ROWS%\2) THEN
      'Unfo buffer still full.
      resetUndo 1
    ENDIF
  ENDIF
END SUB

'Called in case of emergency, and after loading or saving a file.
SUB resetUndo(withPrompt%)
  LOCAL ii%
  
  IF withPrompt% THEN
    LOCAL ok$ = promptForAnyKey$("Undo buffer full and will be cleared. Press any key to continue.")
  ENDIF

  FOR ii% = 0 TO MAX_NUM_UNDOS%-1
    undoIdx% = ii%
    clrUndoEntry
  NEXT ii%
  bufNumRows%(UNDO_BIDX%) = 0
  undoIdx% = 0
  'Hack: boost key count to make sure pending undo records are aborted.
  keyCounter% = keyCounter% + 2
END SUB

SUB saveFile
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL row% = 0
  LOCAL lin$

  promptMsg "Saving...", 1
  OPEN bufFilename$(bIdx%) FOR OUTPUT AS #1

  DO WHILE row% < bufNumRows%(bIdx%)
    rdBufLine(bIdx%, row%, lin$)
    PRINT #1, lin$
    row% = row% + 1
  LOOP

  CLOSE #1

  resetUndo 0
  bufIsModified%(bIdx%) = 0
  promptMsg "File saved.", 1
END SUB

SUB saveKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  IF bufFilename$(bIdx%) = "" THEN
    saveAsKeyHandler
  ELSE
    saveFile
  ENDIF
END SUB

SUB saveAsKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  bufFilename$(bIdx%) = promptForText$("Save Buffer as: ")
  saveFile
END SUB

SUB paasei
  PAGE COPY 0 TO 4
  PAGE WRITE 3  
  CLS RGB(BLACK)
  CIRCLE MM.HRES/2, MM.VRES/2, (MM.VRES/2)-10, 10, 1, RGB(BLACK), RGB(YELLOW)
  CIRCLE MM.HRES/2-100, MM.VRES/2-100, 40, 1, 1/2, RGB(BLACK), RGB(BLACK)
  CIRCLE MM.HRES/2+100, MM.VRES/2-100, 40, 1, 1/2, RGB(BLUE), RGB(BLUE)
  ARC MM.HRES/2, MM.VRES/2, (MM.VRES/4)-5, (MM.VRES/4)+5, 100, 260, RGB(BLACK)
  PAGE WRITE 5
  LOCAL scale!=0.1
  LOCAL newx%, newy%
  DO WHILE scale! < 1
    newx% = MM.HRES*(1 - scale!)/2
    newy% = MM.VRES*(1 - scale!)/2
    PAGE COPY 4 TO 5
    IMAGE RESIZE_FAST 0, 0, MM.HRES, MM.VRES, newx%, newy%, MM.HRES*scale!, MM.VRES*scale!, 3, 1
    PAGE COPY 5 TO 0, B
    scale! = scale! * 1.1
  LOOP
  PAGE COPY 4 TO 0, B
  PAGE WRITE 0
END SUB

SUB helpKeyHandler
  showHelpPopup
  LOCAL dummy$ = promptForAnyKey$("")
  removeHelpPopup
END SUB

SUB screenshotKeyHandler
  LOCAL screenshotFileName$ = promptForText$("Screenshot Filename: ")
  promptMsg "Saving...", 1
  SAVE IMAGE screenshotFileName$
  promptMsg "Screenshot saved.", 1
END SUB

SUB startMacroRecKeyHandler
  macroRecEnabled% = 1
  promptMsg "Macro recording started", 1
END SUB

SUB playMacroKeyHandler
  LOCAL ii%=0
  
  IF macroRecEnabled% THEN
    macroRecEnabled% = 0
    'Remove the last entry which is the stop recording / start playback macro key.
    macroRecordNumEntries% = macroRecordNumEntries% - 1
    promptMsg "Macro recording stopped", 1
    EXIT SUB
  ENDIF

  IF macroRecordNumEntries% = 0 THEN
    promptMsg "No macro on record. Record macro before playback.", 1
    EXIT SUB
  ENDIF
  
  promptMsg "Playing back recorded macro...", 1
  
  DO WHILE ii% < macroRecordNumEntries%
    handleKey macroRecord%(ii%)
    ii% = ii%+1
  LOOP
  
  promptMsg "Macro playback done.", 1
END SUB

'<-- End of Key Handler section

'--> Keypress with modifiers and selection logic.

'Returns true if selection should be cleared by this keyPress
FUNCTION clrSelectionKey%(key%)
  IF isShiftNavKey%(key%) THEN
    clrSelectionKey% = 0
    EXIT FUNCTION
  ENDIF

  SELECT CASE key%
    CASE INDENT_KEY%, UNINDENT_KEY%, COPY_KEY%, UNDO_KEY%
      clrSelectionKey% = 0
    CASE ELSE
      clrSelectionKey% = 1
  END SELECT
END FUNCTION

'Returns true if given keycode is shift + one of the navigation keys
FUNCTION isShiftNavKey%(key%)
  SELECT CASE key%
    CASE SELECT_CRSR_U_KEY%, SELECT_HOME_KEY%, SELECT_END_KEY%, SELECT_PGUP_KEY%, SELECT_PGDOWN_KEY%, SELECT_CRSR_D_KEY%
      isShiftNavKey% = 1
    CASE SELECT_CRSR_L_KEY%, SELECT_CRSR_R_KEY%
      isShiftNavKey% = 1
    CASE ELSE
      isShiftNavKey% = 0
  END SELECT
END FUNCTION

'Key press dispatcher with support for selections and modifiers
SUB handleKey pressedKey%
  STATIC nConsecHomePresses% = 0, nConsecEndPresses% = 0, nSelectConsecPresses% = 0

  IF macroRecEnabled% THEN
    macroRecord%(macroRecordNumEntries%) = pressedKey%
    macroRecordNumEntries% = macroRecordNumEntries% + 1
    IF macroRecordNumEntries% >= MAX_NUM_MACRO_RECORDINGS% THEN
      promptMsg "Max. macro record reached. Disabling recording.", 1
      macroRecEnabled% = 0
    ENDIF
  ENDIF
  
  keyCounter% = keyCounter% + 1

  IF showKeyCodeAtPrompt% THEN
    promptMsg "KeyCode: " + STR$(pressedKey%), 1
  ELSE
    'Remove any messages on the prompt line
    promptMsg "", 0
  ENDIF

  IF isShiftNavKey%(pressedKey%) THEN
    nSelectConsecPresses% = nSelectConsecPresses% + 1
    IF nSelectConsecPresses% = 1 THEN
      winSelectCol%(crsrActiveWidx%) = winBufCrsrCol%(crsrActiveWidx%)
      winSelectRow%(crsrActiveWidx%) = winBufCrsrRow%(crsrActiveWidx%)
    ENDIF
  ENDIF

  IF (pressedKey% = HOME_KEY%) OR (pressedKey% = SELECT_HOME_KEY%) THEN
    nConsecHomePresses% = nConsecHomePresses% + 1
  ELSE
    nConsecHomePresses% = 0
  ENDIF

  IF (pressedKey% = END_KEY%) or (pressedKey% = SELECT_END_KEY%) THEN
    nConsecEndPresses% = nConsecEndPresses% + 1
  ELSE
    nConsecEndPresses% = 0
  ENDIF

  SELECT CASE pressedKey%
    CASE CRSR_UP_KEY%, SELECT_CRSR_U_KEY%
      crsrUpKeyHandler
    CASE CRSR_DOWN_KEY%, SELECT_CRSR_D_KEY%
      crsrDownKeyHandler
    CASE CRSR_LEFT_KEY%, SELECT_CRSR_L_KEY%
      crsrLeftKeyHandler
    CASE CRSR_RIGHT_KEY%, SELECT_CRSR_R_KEY%
      crsrRightKeyHandler
    CASE HOME_KEY%, SELECT_HOME_KEY%
      homeKeyHandler nConsecHomePresses%
    CASE END_KEY%, SELECT_END_KEY%
      endKeyHandler nConsecEndPresses%
    CASE PGUP_KEY%, SELECT_PGUP_KEY%
      pgUpKeyHandler
    CASE PGDOWN_KEY%, SELECT_PGDOWN_KEY%
      pgDownKeyHandler
    CASE EXIT_KEY%
      exitKeyHandler
    CASE TOGGLE_SCREEN_SPLIT_KEY%
      toggleScreenSplitKeyHandler
    CASE TOGGLE_ACTIVE_WINDOW_KEY%
      toggleActiveWindowKeyHandler
    CASE TOGGLE_INS_OVR_MODE_KEY%
      toggleInsOvrModeKeyHandler
    CASE TOGGLE_BUFFER_KEY%
      toggleBufferKeyHandler
    CASE LOAD_INTO_CURRENT_BUF_KEY%
      loadIntoCurrentBufKeyHandler
    CASE CLOSE_BUFFER_KEY%
      closeBufferKeyHandler
    CASE ENTER_KEY%
      enterKeyHandler
    CASE BACKSPACE_KEY%
      backspaceKeyHandler
    CASE DELETE_KEY%
      deleteKeyHandler
    CASE INDENT_KEY%
      indentKeyHandler
    CASE UNINDENT_KEY%
      unIndentKeyHandler
    CASE TOGGLE_SHOW_KEYCODE_AT_PROMPT%
      showKeyCodeAtPrompt% = NOT showKeyCodeAtPrompt%
      IF showKeyCodeAtPrompt% THEN
        promptMsg "Showing keycodes at prompt. Toggle with Alt-K", 1
      ENDIF
    CASE GOTO_KEY%
      gotoKeyHandler
    CASE CUT_KEY%
      cutKeyHandler
    CASE COPY_KEY%
      copyKeyHandler
    CASE PASTE_KEY%
      pasteKeyHandler
    CASE FIND_KEY%
      findKeyHandler
    CASE FIND_NEXT_KEY%
      findNextKeyHandler
    CASE REPLACE_KEY%
      replaceKeyHandler
    CASE UNDO_KEY%
      undoKeyHandler
    CASE SAVE_KEY%
      saveKeyHandler
    CASE SAVE_AS_KEY%
      saveAsKeyHandler
    CASE 261
      paasei
    CASE HELP_KEY%
      helpKeyHandler
    CASE SCREENSHOT_KEY%
      screenshotKeyHandler
    CASE START_MACRO_REC_KEY%
      startMacroRecKeyHandler
    CASE PLAY_MACRO_KEY%
      playMacroKeyHandler
    CASE ELSE
      pressedKey% = pressedKey% AND 255
      IF isPrintable%(pressedKey%) THEN
        'This is for the non-ctrl keys, i.e. the edits.
        editKeyHandler CHR$(pressedKey%)
      ENDIF
  END SELECT

  IF clrSelectionKey%(pressedKey%) THEN
    'Clear selection
    IF selectMode%(crsrActiveWidx%) THEN
      winRequestRedraw%(crsrActiveWidx%) = 1 
    ENDIF

    nSelectConsecPresses% = 0
    winSelectCol%(crsrActiveWidx%) = -1
    winSelectRow%(crsrActiveWidx%) = -1
  ENDIF

  crsrOn
END SUB

'This is essentially a replacement for INKEY, with support for modifiers.
SUB checkKeyAndModifier
  STATIC k% = 0, prevk% = 0
  STATIC m% = 0
  STATIC nextPollTime%=0
  STATIC repeatCount% = 0

  prevk% = k%

  m% = KEYDOWN(7)
  IF (m% AND LCTRL_MASK%) OR (m% AND RCTRL_MASK%) THEN
    m% = 1<<KEYCODE_CTRL_BITPOS%
  ELSEIF (m% AND LALT_MASK%) OR (m% AND RALT_MASK%) THEN
    m% = 1<<KEYCODE_ALT_BITPOS%
  ELSEIF (m% AND LSHFT_MASK%) OR (m% AND RSHFT_MASK%) THEN
    m% = 1<<KEYCODE_SHFT_BITPOS%
  ENDIF

  k% = KEYDOWN(1)

  IF k% = 0 THEN 'No key pressed. We're done.
    EXIT SUB
  ENDIF

  IF k% <> prevk% THEN
    repeatCount% = 0
  ENDIF

  IF (UCASE$(CHR$(k%)) <> UCASE$(CHR$(prevk%))) OR (TIMER > nextPollTime%) THEN
    repeatCount% = repeatCount% + 1
    IF repeatCount% = 1 THEN 'We enter this case when we just pressed a new key.
      nextPollTime% = TIMER + KEYB_REPEAT_FIRST% 'Use REPEAT_FIRST for the initial repeat time.
    ELSE
      nextPollTime% = TIMER + KEYB_REPEAT_REST% 'Use REPEAT_REST for the subsequent repeat times.
    ENDIF
    handleKey k% OR m%
  ENDIF
END SUB

        