OPTION EXPLICIT
OPTION DEFAULT NONE
OPTION BASE 0
OPTION CONSOLE BOTH

#INCLUDE "settings_dontedit.INC"

IF MM.INFO(MODE) = 1.8 THEN  
  PAGE COPY 0 TO 2 'We keep a copy of the console screen on page 2.
ELSE
  MODE 1, 8
  PAGE WRITE 2
  CLS
  PAGE WRITE 0  
ENDIF

FONT 1, 1

CONST VERSION$ = "0.10"

#INCLUDE "keybindings_dontedit.INC"

CONST MAX_NUM_ROWS% = 14000 'This is the total number of lines available, across all buffers, including clipboard and undo buffer.
CONST NUM_BUFFERS% = 4 'Two regular buffers, the clipboard and the undo buffer.
CONST MAX_NUM_WINDOWS% = 2
CONST MAX_NUM_UNDOS% = 32
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
CONST SLIDER_WIDTH% = 4

CONST DOUBLE_QUOTE$ = CHR$(34)
CONST CRSR_RIGHT_INKEY$ = CHR$(131)
CONST CRSR_LEFT_INKEY$ = CHR$(130)
CONST RIGHT_ARROW$ = CHR$(148)
CONST LEFT_ARROW$ = CHR$(149)

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

'--> Window redraw actions
CONST NO_REDRAW% = 0
CONST FULL_REDRAW% = 1
CONST SCROLL_UP% = 2
CONST SCROLL_DOWN% = 3
CONST SCROLL_LEFT% = 4
CONST SCROLL_RIGHT% = 5
'<--

#INCLUDE "Ultrabox.inc"
#INCLUDE "xFind.INC"

DIM winContentScrollDownStartRow%, winContentScrollUpStartRow%
DIM winContentScrollUpEndRow%

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
DIM bufSavedTopCol%(NUM_BUFFERS%-1)
DIM bufSavedTopRow%(NUM_BUFFERS%-1)
DIM bufSynHLEnabled%(NUM_BUFFERS%-1)
DIM bufIsConsole%(NUM_BUFFERS%-1)

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
DIM winRedrawAction%(MAX_NUM_WINDOWS%-1)
DIM winVisible%(MAX_NUM_WINDOWS%-1) 'True is window is visible.
DIM winBuf%(MAX_NUM_WINDOWS%-1) 'Associated buffer.
'<--
initAllWindows

'Used by string allocator. Each bit corresponds to one string in theStrings array below
DIM allocatorBitArray%(MAX_NUM_ROWS%/64)

'A pool of strings used by the buffer accessors. The buffers themselves don't hold the actual strings.
'They hold indices to this string pool. Strings must be allocated to the buffer before use, and released when no longer needed.
DIM theStrings$(MAX_NUM_ROWS%)

'Prealloc 1st string as empty string
allocatorBitArray%(0) = 1
theStrings$(0) = ""

'--> Cursor Sprite definitions
DIM CRSR_INS_SPRITE%(COL_WIDTH%*ROW_HEIGHT%-1) 'Insert cursor sprite
CRSR_INS_SPRITE_DATA: 
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0
  DATA FG_COLOR%, FG_COLOR%, 0, 0, 0, 0, 0, 0

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
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
  DATA 0, 0, 0, 0, 0, 0, 0, 0
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

'--> Syntax highlighting data structures:
DIM parseWhiteSpaces$ = " ,%$!()=<>/\*+-:"

CONST PARSE_STATE_INITIAL%=0
CONST PARSE_STATE_WHITESPACE%=1
CONST PARSE_STATE_WORD%=2
CONST PARSE_STATE_COMMENT%=3
CONST PARSE_STATE_STRING%=4

DIM NUM_KEYWORDS% = 0 'Correct value will be determined at runtime by scanCmdList below.
KEYWORD_LIST_DATA:
  DATA " ,%$!()=<>/\*+-:" 'Whitespaces
  DATA "ADC","BOX","CLEAR","CONTINUE","DATA","DO","END","SELECT","EXECUTE","EXIT","SUB","FUNCTION"
  DATA "I2C3","IRETURN","LIST","MAP","MKDIR","ONEWIRE","PIN","PORT","READ","RMDIR","SERVO","SPI2"
  DATA "TEXT","TURTLE","WII","ARC","CASE","CLOSE","COPY","DATE","EDIT","CSUB","FILES","GOSUB","IF"
  DATA "KILL","LOAD","MAP","MODE","OPEN","PIXEL","PRINT","REM","RUN","SETPIN","SPRITE","TIME"
  DATA "UPDATE","FIRMWARE","XMODEM","AUTOSAVE","CASE","ELSE","CLS","CPU","DEFINEFONT","ENDIF"
  DATA "FONT","GOTO", "IMAGE","LET","LOCAL","MATH","NEW","OPTION","PLAY","PULSE","RENAME","SAVE"
  DATA "SETTICK","STATIC", "TIMER","VAR","BITBANG","CHDIR","COLOUR","DHT22","ERASE"
  DATA "FOR","I2C","INPUT","LINE","LONGSTRING","MEMORY","NEXT","PAGE","POKE","PWM","RESTORE"
  DATA "SEEK","SORT","TRACE","WATCHDOG","BLIT","CIRCLE","CONST","DAC","DIM","ELSEIF","ERROR"
  DATA "FRAMEBUFFER","I2C2","IR","LOOP","MID","ON","PAUSE","POLYGON","RBOX","RETURN","SPI"
  DATA "TEMPR","START","TRIANGLE","WHILE","THEN","INSTR","CHR","ASC","VAL","STR","AND"
  DATA "OR","XOR","INV","INT","WRITE","SHOW","NOT","SKIP","LOADARRAY","SPACE","RGB"
  DATA "LEN","LEFT","RIGHT","EOF","MAX","MIN","COLOR","UCASE","LCASE","LCOMPARE","HIDE"
  DATA "SAFE", "MM", "INFO", "DEVICE", "ERRNO", "ERRMSG", "HRES", "VRES", "PEEK", "MOD"
  DATA "INTEGER", "STRING", "FLOAT", "OFF", "OUTPUT", "RANDOM", "REPLACE", "#COMMENT"
  DATA "INCLUDE", "TO", "AS", "LENGTH", "UNTIL","IR","LS", "STEP", "STOP", "MAX PAGES"
  DATA "ABS", "ACOS", "ASC", "ASIN", "ATAN2", "ATN","BAUDRATE","BIN", "MM.INFO", "SCROLL"
  DATA "BIN2STR", "BOUND", "CINT", "CLASSIC", "COS","CWD", "CAT", "#INCLUDE", "VARADDR"
  DATA "DATETIME", "DAY", "DEG", "DIR", "DISTANCE", "EPPOCH", "EVAL", "EXP", "CALL", "MM.HRES"
  DATA "FIELD", "FIX", "FORMAT", "GETSCANLINE", "GPS", "HEX", "INKEY", "KEYDOWN", "MM.VRES"
  DATA "LGETBYTE", "LGETSTR", "LINSTR", "LLEN", "LOC", "LOF", "LOG", "NUNCHUK", "OCT"
  DATA "PI", "PULSIN", "RAD", "RND", "SGN", "SIN","STR2BIN", "SQR", "STR", "TAB", "TAN", "POS"
  DATA "INC", "RESIZE", "TRANSPARENCY", "CONCAT", "SETBYTE", "endSentinel" 'Keep this at the end of the list. Do not delete.
   
SUB scanCmdList
  LOCAL cmd$

  RESTORE KEYWORD_LIST_DATA
  DO
    READ cmd$
    NUM_KEYWORDS% = NUM_KEYWORDS%+1
  LOOP UNTIL cmd$ = "endSentinel"
END SUB
scanCmdList

DIM KEYWORD_LIST$(NUM_KEYWORDS%-1) LENGTH 16

SUB initCmdList
  LOCAL ii%
  
  RESTORE KEYWORD_LIST_DATA  
  FOR ii%=0 TO NUM_KEYWORDS%-1
    READ KEYWORD_LIST$(ii%) 
  NEXT ii%
END SUB
initCmdList

CONST PARSE_POS% = 0
CONST PARSE_STATE% = 1
CONST PARSE_COLOR_FG% = 2
CONST PARSE_COLOR_KEYWORD% = 3
CONST PARSE_COLOR_STRING% = 4
CONST PARSE_COLOR_COMMENT% = 5
CONST PARSER_NUM_KEYWORDS% = 6
CONST PARSER_FRAG_START% = 7
CONST PARSER_START_COL% = 8 'One based start column
CONST PARSER_END_COL% = 9 'One based end column
CONST PARSER_LINE_X% = 10
CONST PARSER_LINE_Y% = 11

DIM parserCSUBCtxt%(PARSER_LINE_Y%) = (1, PARSE_STATE_INITIAL%, FG_COLOR%, KEYWORD_COLOR%, STRING_COLOR%, COMMENT_COLOR%, NUM_KEYWORDS%, 1, 1, 1, 0, 0)
'<--

DIM selectionActive%=0 'SERIAL input mode only
DIM blinkCursorFlag% = 0
DIM showKeyCodeAtPrompt% = 0
DIM keyCounter% = 0
DIM exitRequested% = 0
DIM splitMode% = NO_SPLIT%
DIM strToFind$ = "" 'For the find function.
DIM executeOnExit$ = "" 'If not empty, name of file to execute when exiting xedit.

DIM crsrMode% = CRSR_MODE_INS%  'Insert or overwrite mode
DIM crsrActiveWidx% = 0 'Active window index.
DIM crsrState% = 0 '1 or 0, On or Off.
DIM prevCrsrY% = 0 'Previous crsr position to detect when sprite has to be redrawn.
DIM prevCrsrX% = 0

#INCLUDE "csubs.INC"

PAGE WRITE 0
COLOR FG_COLOR%, BG_COLOR%
CLS

setCrsrSprite

drawGenFooter
initWindow 0, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%, 0
drawWindow 0

SUB setupCtxt
  LOCAL dummy%, bIdx%
  LOCAL cmdLineArgs$(MAX_NUM_CMDLINE_ARGS%)
  LOCAL nArgs%

  parseCmdLine(MM.CMDLINE$, cmdLineArgs$(), nArgs%)

  IF nArgs% > 0 THEN
    dummy% = checkAndLoad%(0, cmdLineArgs$(0), NOT DISABLE_CONFIRMATION_PROMPTS%) 'File to edit can be passed in on command line.
    drawWindow 0
  ENDIF
  IF nArgs% > 1 THEN
    dummy% = checkAndLoad%(1, cmdLineArgs$(1), NOT DISABLE_CONFIRMATION_PROMPTS%)
  ENDIF

  IF nArgs%=0 THEN
    IF RESTORE_PREV_SESSION_CTXT% THEN
      restoreSessionCtxt  
      IF (bufFilename$(0) <> "") AND NOT bufIsConsole%(0) THEN
        dummy% = checkAndLoad%(0, bufFilename$(0), NOT DISABLE_CONFIRMATION_PROMPTS%)
      ENDIF
      IF (bufFilename$(1) <> "") AND NOT bufIsConsole%(1) THEN
        dummy% = checkAndLoad%(1, bufFilename$(1), NOT DISABLE_CONFIRMATION_PROMPTS%)
      ENDIF
      
      restoreBufPos
  
      drawWindow 0
    ENDIF
  ENDIF
END SUB
setupCtxt

SETTICK CURSOR_BLINK_PERIOD%, blinkCursorInt, 1

mainLoop

EndOfProg:

'Disable IRQ
SETTICK 0, 0, 1

IF RESTORE_PREV_SESSION_CTXT% THEN
  saveSessionCtxt
ENDIF

CLS RGB(BLACK)

IF executeOnExit$ <> "" THEN
  executeOnExit$ = "RUN "+DOUBLE_QUOTE$+executeOnExit$+DOUBLE_QUOTE$
  EXECUTE executeOnExit$
ENDIF

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
        drawSlider wIdx%
        
        SELECT CASE winRedrawAction%(wIdx%)
          CASE FULL_REDRAW%
            drawWinContents wIdx%
          CASE SCROLL_UP%
            winContentsScrollUp wIdx%, winContentScrollUpStartRow%, winContentScrollUpEndRow%
          CASE SCROLL_DOWN%
            winContentsScrollDown wIdx%, winContentScrollDownStartRow%
          CASE SCROLL_LEFT%
            winContentsScrollLeft wIdx%
          CASE SCROLL_RIGHT%
            winContentsScrollRight wIdx%            
        END SELECT
        winRedrawAction%(wIdx%) = NO_REDRAW%
      ENDIF
    NEXT wIdx%

    crsrDraw
  LOOP  
END SUB

SUB restoreSessionCtxt
  LOCAL lin$

  IF DIR$(CTXT_FILE_PATH$) <> "" THEN
    OPEN CTXT_FILE_PATH$ FOR INPUT AS #1

    'Version check
    LINE INPUT #1, lin$
    IF lin$ <> "VERSION="+VERSION$ THEN
      CLOSE #1
      EXIT SUB
    ENDIF
    
    LINE INPUT #1, lin$
    bufFilename$(0) = lin$
  
    LINE INPUT #1, lin$  
    bufSavedCrsrCol%(0) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufSavedCrsrRow%(0) = VAL(lin$)

    LINE INPUT #1, lin$  
    bufSavedTopCol%(0) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufSavedTopRow%(0) = VAL(lin$)

    LINE INPUT #1, lin$
    bufSynHLEnabled%(0) = VAL(lin$)

    LINE INPUT #1, lin$
    bufIsConsole%(0) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufFilename$(1) = lin$
    
    LINE INPUT #1, lin$
    bufSavedCrsrCol%(1) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufSavedCrsrRow%(1) = VAL(lin$)

    LINE INPUT #1, lin$
    bufSavedTopCol%(1) = VAL(lin$)
    
    LINE INPUT #1, lin$
    bufSavedTopRow%(1) = VAL(lin$)

    LINE INPUT #1, lin$
    bufSynHLEnabled%(1) = VAL(lin$)

    LINE INPUT #1, lin$
    bufIsConsole%(1) = VAL(lin$)

    LINE INPUT #1, lin$
    winBuf%(crsrActiveWidx%) = VAL(lin$)

    LINE INPUT #1, lin$
    winBuf%(NOT crsrActiveWidx%) = VAL(lin$)
        
    CLOSE #1
  ENDIF
END SUB

SUB saveSessionCtxt
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)

  'Save active window's cursor position (the other one is already saved.
  bufSavedCrsrCol%(bIdx%) = winBufCrsrCol%(crsrActiveWidx%)
  bufSavedCrsrRow%(bIdx%) = winBufCrsrRow%(crsrActiveWidx%)
  bufSavedTopCol%(bIdx%) = winBufTopCol%(crsrActiveWidx%)
  bufSavedTopRow%(bIdx%) = winBufTopRow%(crsrActiveWidx%)

  OPEN CTXT_FILE_PATH$ FOR OUTPUT AS #1
  
  PRINT #1, "VERSION="+VERSION$
  PRINT #1, bufFilename$(0)
  PRINT #1, STR$(bufSavedCrsrCol%(0))
  PRINT #1, STR$(bufSavedCrsrRow%(0))
  PRINT #1, STR$(bufSavedTopCol%(0))
  PRINT #1, STR$(bufSavedTopRow%(0))  
  PRINT #1, STR$(bufSynHLEnabled%(0))
  PRINT #1, STR$(bufIsConsole%(0))
  
  PRINT #1, bufFilename$(1)
  PRINT #1, STR$(bufSavedCrsrCol%(1))
  PRINT #1, STR$(bufSavedCrsrRow%(1))
  PRINT #1, STR$(bufSavedTopCol%(1))
  PRINT #1, STR$(bufSavedTopRow%(1))  
  PRINT #1, STR$(bufSynHLEnabled%(1))
  PRINT #1, STR$(bufIsConsole%(1))

  PRINT #1, STR$(winBuf%(crsrActiveWidx%))
  PRINT #1, STR$(winBuf%(NOT crsrActiveWidx%))
  
  CLOSE #1
END SUB

'Popup is prepared on a separate page in a Box, then shown on page 0 using blit.
SUB showKeybindPopup                                                                
  LOCAL longestStringLen% = LEN("Key Bindings (Ref. Key Bindings section in XEdit.bas to modify):")
  LOCAL numLines% = 40
  LOCAL boxWidth% = (longestStringLen%+4)*COL_WIDTH%
  LOCAL boxHeight% = (numLines%+4)*ROW_HEIGHT%

  PAGE WRITE 3
  CLS
  
  COLOR BG_COLOR%, FG_COLOR2%
  BOX 0, 0, boxWidth%, boxHeight%, 4, BG_COLOR2%, FG_COLOR2%
  
  LOCAL x% = 2*COL_WIDTH%
  LOCAL y% = 2*ROW_HEIGHT%

  LOCAL title$ = "XEdit Help"
  TEXT x%, y%, SPACE$((longestStringLen% - LEN(title$))\2) + title$
  y% = y% + 2*ROW_HEIGHT%

  TEXT x%, y%, "Key Bindings (Ref. Key Bindings section in XEdit.bas to modify):"
  y% = y% + 2*ROW_HEIGHT%                  
  TEXT x%, y%, "F1          = Help"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F2/F9       = Save File/Save File as"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F3          = Load File"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F12         = Close File"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F4          = Toggle Buffer"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F5          = Toggle Window split"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F6          = Show Console Screen in current buffer"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F10         = Exit XEdit"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F11         = Exit XEdit and run program currently in buffer"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-O      = Toggle Active Window"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl/Alt-F  = Forward/Reverse Find Prompt or Selection"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl/Alt-N  = Find Next/Previous"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-S      = Find Across Files (xFind)"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-R      = Replace Prompt or Selection"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-X/Y/V  = Cut/Copy/Paste"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-K      = Delete from cursor to End Of Line"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-G      = Goto Line"
  y% = y% + ROW_HEIGHT%
  IF SERIAL_INPUT_COMPAT_MODE% = 0 THEN
    TEXT x%, y%, "INS         = Toggle Insert/Overwrite mode"
  ELSE
    TEXT x%, y%, "Ctrl-W      = Toggle Insert/Overwrite mode"
  ENDIF
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Home 1/2/3x = Go To Start of Line/Page/Buffer"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "End  1/2/3x = Go To End of Line/Page/Buffer"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-M      = Scroll current line to Center of Window"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Alt-M       = Scroll current line to Top of Window"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Tab/Shift-Tab = Indent/Unindent Line/Selection"
  y% = y% + ROW_HEIGHT%
  IF SERIAL_INPUT_COMPAT_MODE% = 0 THEN
    TEXT x%, y%, "Shift-Navigation Key = Start/Extend Selection"
  ELSE
    TEXT x%, y%, "Esc         = Toggle Selection Mode On/Off"
  ENDIF
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-A      = Select All"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Ctrl-Z      = Undo"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F7          = Start Macro Recording"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "F8          = Stop Macro Recording / Playback recorded macro"
  y% = y% + ROW_HEIGHT%
  IF SERIAL_INPUT_COMPAT_MODE% = 0 THEN
    TEXT x%, y%, "Alt-C       = Toggle Syntax Highlighting On/Off"
  ELSE
    TEXT x%, y%, "CTRL-S      = Toggle Syntax Highlighting On/Off"
  ENDIF
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Alt-K       = Show Key Code at prompt"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Alt-R       = Show XEdit Resource Utilization."
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Alt-S       = Screenshot"
  y% = y% + 2*ROW_HEIGHT%
  TEXT x%, y%, "Navigation Keys: Cursor Up/Down/Left/Right, PgUp/Down, Home/End"
  y% = y% + 2*ROW_HEIGHT%  
  title$ =  "Press "+RIGHT_ARROW$+" to go to the User Config. Settings Help Page."
  TEXT x%, y%, SPACE$((longestStringLen% - LEN(title$))\2) + title$

  'Copy screen to page 1 so we can restore it later.
  PAGE COPY 0 TO 1  
  PAGE WRITE 0
  COLOR FG_COLOR%, BG_COLOR%

  BLIT 0, 0, MM.HRES/2 - boxWidth%/2, MM.VRES/2 - boxHeight%/2, boxWidth%, boxHeight%, 3
END SUB

'Popup is prepared on a separate page in a Box, then shown on page 0 using blit.
SUB showUserCfgPopup
  LOCAL longestStringLen% = LEN("settings.user.inc and make your changes. Same with keybinding.user.inc.")
  LOCAL numLines% = 18
  LOCAL boxWidth% = (longestStringLen%+4)*COL_WIDTH%
  LOCAL boxHeight% = (numLines%+4)*ROW_HEIGHT%

  PAGE WRITE 3
  CLS
  
  COLOR BG_COLOR%, FG_COLOR2%
  BOX 0, 0, boxWidth%, boxHeight%, 4, BG_COLOR2%, FG_COLOR2%
  
  LOCAL x% = 2*COL_WIDTH%
  LOCAL y% = 2*ROW_HEIGHT%

  LOCAL title$ = "XEdit Help (continued)"
  TEXT x%, y%, SPACE$((longestStringLen% - LEN(title$))\2) + title$
  y% = y% + 2*ROW_HEIGHT%

  TEXT x%, y%, "User Configurable Settings (Set at start of XEdit.bas):"
  y% = y% + 2*ROW_HEIGHT%
  TEXT x%, y%, "SEARCH_IS_CASE_SENSITIVE%=0/1      Default=0"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "TAB_WIDTH%=<Num.>                  Default=2"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "RESTORE_PREV_SESSION_CTXT%=0/1     Default=0"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "FG/KEYWORD/STRING/COMMENT/BG_COLOR%"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "DISABLE_CONFIRMATION_PROMPTS%=0/1  Default=0"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "NUM_BACKUP_FILES%=<Num.>           Default=0"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "ENABLE_FILE_DIALOG_BOX%=0/1        Default=1"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "SERIAL_INPUT_COMPAT_MODE%=0/1      Default=0"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "(Alt-based key bindings N/A in SERIAL_INPUT_COMPAT_MODE)"
  y% = y% + 2*ROW_HEIGHT%
  TEXT x%, y%, " To make settings or keybinding changes, copy settings.default.inc to"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "settings.user.inc and make your changes. Same with keybinding.user.inc."
  y% = y% + 2*ROW_HEIGHT%

  title$ = "Press "+LEFT_ARROW$+" to go to the Key Bindings Help Page."
  TEXT x%, y%, SPACE$((longestStringLen% - LEN(title$))\2) + title$  
  y% = y% + ROW_HEIGHT%

  'Copy screen to page 1 so we can restore it later.
  PAGE COPY 0 TO 1  
  PAGE WRITE 0
  COLOR FG_COLOR%, BG_COLOR%

  BLIT 0, 0, MM.HRES/2 - boxWidth%/2, MM.VRES/2 - boxHeight%/2, boxWidth%, boxHeight%, 3
END SUB

SUB removePopup
  PAGE COPY 1 TO 0
END SUB

'Popup is prepared on a separate page in a Box, then shown on page 0 using a sprite.
SUB showResUtilPopup                                                                
  LOCAL longestStringLen% = LEN("Clipboard Buffer Utilization = 12000/12000 (100.00%)")
  LOCAL numLines% = 7
  LOCAL boxWidth% = (longestStringLen%+4)*COL_WIDTH%
  LOCAL boxHeight% = (numLines%+4)*ROW_HEIGHT%
  LOCAL linePoolUtil% = 0, ii%=0

  'Copy screen to page 1 so we can restore it later.
  PAGE COPY 0 TO 1  
  
  promptMsg "Computing resource utilization...", 1

  LOCAL buf0n% = bufNumRows%(0)
  LOCAL buf1n% = bufNumRows%(1)
  LOCAL clpn% = bufNumRows%(CLIPBOARD_BIDX%)
  LOCAL undon% = bufNumRows%(UNDO_BIDX%)

  LOCAL buf0pct! = 100*buf0n%/MAX_NUM_ROWS%
  LOCAL buf1pct! = 100*buf1n%/MAX_NUM_ROWS%
  LOCAL clppct! = 100*clpn%/MAX_NUM_ROWS%
  LOCAL undopct! = 100*undon%/MAX_NUM_ROWS%
  
  DO WHILE ii% < MAX_NUM_ROWS%
    IF isAllocated%(ii%) THEN
      linePoolUtil% = linePoolUtil% + 1
    ENDIF
    ii% = ii%+1
  LOOP

  LOCAL linepct! = 100*linePoolUtil%/MAX_NUM_ROWS%
  
  PAGE WRITE 3
  CLS
  
  COLOR BG_COLOR%, FG_COLOR2%
  BOX 0, 0, boxWidth%, boxHeight%, 4, BG_COLOR2%, FG_COLOR2%
  
  LOCAL x% = 2*COL_WIDTH%
  LOCAL y% = 2*ROW_HEIGHT%

  LOCAL title$ = "XEdit Resource Utilization"
  TEXT x%, y%, SPACE$((longestStringLen% - LEN(title$))\2) + title$
  y% = y% + 2*ROW_HEIGHT%

  TEXT x%, y%, "Buffer 1 Utilization         = " + FORMAT$(buf0n%, "%5g") + "/" + STR$(MAX_NUM_ROWS%) + " (" + FORMAT$(buf0pct!,"%5.2f") + "%)"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Buffer 2 Utilization         = " + FORMAT$(buf1n%, "%5g") + "/" + STR$(MAX_NUM_ROWS%) + " (" + FORMAT$(buf1pct!,"%5.2f") + "%)"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Clipboard Buffer Utilization = " + FORMAT$(clpn%, "%5g") + "/" + STR$(MAX_NUM_ROWS%) + " (" + FORMAT$(clppct!,"%5.2f") + "%)"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Undo Buffer Utilization      = " + FORMAT$(undon%, "%5g") + "/" + STR$(MAX_NUM_ROWS%) + " (" + FORMAT$(undopct!,"%5.2f") + "%)"
  y% = y% + ROW_HEIGHT%
  TEXT x%, y%, "Line Pool Utilization        = " + FORMAT$(linePoolUtil%, "%5g") + "/" + STR$(MAX_NUM_ROWS%) + " (" + FORMAT$(linepct!,"%5.2f") + "%)"

  PAGE WRITE 0
  COLOR FG_COLOR%, BG_COLOR%

  BLIT 0, 0, MM.HRES/2 - boxWidth%/2, MM.VRES/2 - boxHeight%/2, boxWidth%, boxHeight%, 3
END SUB

SUB parseCmdLine(cmdLine$, cmdLineArgs$(), nArgs%)
  LOCAL curPos%=1, startPos%
  LOCAL inWhiteSpace%=1
  LOCAL curArg%=0
  
  DO WHILE (curPos%<=LEN(cmdLine$)) AND (curArg%<MAX_NUM_CMDLINE_ARGS%)
    IF inWhiteSpace% THEN
      IF MID$(cmdLine$, curPos%, 1) <> " " THEN
        startPos% = curPos%
        inWhiteSpace% = 0
      ENDIF
    ELSE
      IF MID$(cmdLine$, curPos%, 1) = " " THEN
        cmdLineArgs$(curArg%) = MID$(cmdLine$, startPos%, curPos%-startPos%)
        curArg% = curArg% + 1
        inWhiteSpace% = 1
      ENDIF
    ENDIF
    curPos% = curPos%+1
  LOOP
  
  IF (inWhiteSpace%=0) AND (curArg% < MAX_NUM_CMDLINE_ARGS%) THEN
    cmdLineArgs$(curArg%) = MID$(cmdLine$, startPos%)
    curArg% = curArg% + 1
  ENDIF
  
  nArgs% = curArg%
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
    prevCrsrY% = crsrY%
    prevCrsrX% = crsrX%
    crsrState% = 1
  ENDIF
END SUB

SUB crsrOff
  IF crsrState% THEN
    SPRITE SHOW CRSR_SPRITE_ID%, MM.HRES-COL_WIDTH%, MM.VRES-ROW_HEIGHT%, 1, 0
    prevCrsrY% = MM.HRES-COL_WIDTH%
    prevCrsrX% = MM.VRES-ROW_HEIGHT%

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
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)            

  IF bufIsConsole%(bIdx%) THEN
    crsrOff
  ELSE
    STATIC crsrOnOff%=0
  
    crsrOnOff% = NOT crsrOnOff%
  
    IF crsrOnOff% THEN
      crsrOn
    ELSE
      crsrOff
    ENDIF
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
  IF linePtr% THEN
    setAllocated(linePtr%, 0)
    linePtr% = 0
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
  
  FOR rowl%=bufNumRows%(bIdx%) TO bufNumRows%(bIdx%)+numRows%-1
    bufLinePtrs%(rowl%, bIdx%) = 0
  NEXT rowl%

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
    bufLinePtrs%(rowl%, bIdx%) = 0
  NEXT rowl%

  bufNumRows%(bIdx%) = bufNumRows%(bIdx%) + numRows%
  bufIsModified%(bIdx%) = 1
  insertBufLines% = 1
END FUNCTION

'Write a line into buffer bIdx, allocate line if needed. Returns false if failed.
FUNCTION wrBufLine%(bIdx%, row%, lin$)
  LOCAL linePtr% = bufLinePtrs%(row%, bIdx%)
  IF NOT linePtr% THEN
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
  IF (bidx%<0) OR (bIdx% >= NUM_BUFFERS%) OR (row% < 0) OR (row% > MAX_NUM_ROWS%) THEN
    PRINT "Error (1)"
    PRINT bIdx%
    PRINT row%
    END
  ENDIF
  IF (bufLinePtrs%(row%, bIdx%) < 0) OR (bufLinePtrs%(row%, bIdx%) > MAX_NUM_ROWS%) THEN
    PRINT "Error (2)"
    PRINT bufLinePtrs%(row%, bIdx%)
    END
  ENDIF
  
  lin$ = theStrings$(bufLinePtrs%(row%, bIdx%))
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
  bufSavedTopCol%(bIdx%) = 0
  bufSavedTopRow%(bIdx%) = 0  
  bufIsConsole%(bIdx%) = 0
  
  FOR ii%=0 TO MAX_NUM_ROWS%-1
    bufLinePtrs%(ii%, bIdx%) = 0
  NEXT ii%
END SUB

SUB setupBuffer(bIdx%, numRows%, filename$, isConsole%) 
  bufNumRows%(bIdx%) = numRows%
  bufFilename$(bIdx%) = filename$
  bufIsConsole%(bIdx%) = isConsole%
  bufIsModified%(bIdx%) = 0
  
  LOCAL fileExt$ = UCASE$(RIGHT$(filename$,4))
  IF (fileExt$=".INC") OR (fileExt$=".BAS") OR (fileExt$="") THEN
    bufSynHLEnabled%(bIdx%) = DEFAULT_ENABLE_SYN_HL%
  ELSE
    bufSynHLEnabled%(bIdx%) = 0
  ENDIF
END SUB

'Empty the keyboard input buffer
SUB emptyInputBuffer
  DO WHILE KEYDOWN(1) <> 0
  LOOP
END SUB

'If on%=1, prompt text is shown. If on%=0 prompt text is removed.
SUB promptMsg(txt$, on%)
  IF on%=1 THEN
    TEXT 0, PROMPTY%, txt$ + SPACE$(MM.HRES\COL_WIDTH% - LEN(txt$))
    emptyInputBuffer
  ELSE
    TEXT 0, PROMPTY%, SPACE$(MM.HRES\COL_WIDTH%)
  ENDIF
END SUB

'Prints the given text on the prompt line, then waits for the user to press any key. 
'The pressed key is returned to the caller.
FUNCTION promptForAnyKey$(txt$)
  LOCAL pressedKey$
  LOCAL latchedTime% = INT(TIMER)

  TEXT 0, PROMPTY%, txt$ + SPACE$(FULL_SCREEN_WINDOW_W%\COL_WIDTH% - LEN(txt$))
  LOCAL crsrPos% = (LEN(txt$)+1)*COL_WIDTH%
  LOCAL invert% = 0

  emptyInputBuffer

  'An overly complex way of getting a blinking cursor at the prompt...
  DO 
    pressedKey$ = INKEY$ 
          
    IF (INT(TIMER) > latchedTime% + CURSOR_BLINK_PERIOD%) THEN
      IF invert% THEN
        BOX crsrPos%, PROMPTY%, COL_WIDTH%, ROW_HEIGHT%, 0, BG_COLOR%, BG_COLOR%
      ELSE
        BOX crsrPos%, PROMPTY%, COL_WIDTH%, ROW_HEIGHT%, 0, FG_COLOR%, FG_COLOR%
      ENDIF
      invert% = invert% XOR 1
      latchedTime% = INT(TIMER)
    ENDIF
  LOOP UNTIL pressedKey$ <> ""

  TEXT 0, PROMPTY%, SPACE$(MM.HRES\COL_WIDTH%)

  emptyInputBuffer

  promptForAnyKey$ = pressedKey$
END FUNCTION

'Prints the given text on the prompt line, then waits for input. 
'The input string is returned to the caller.
FUNCTION promptForText$(txt$)
  LOCAL inputStr$
  TEXT 0, PROMPTY%, txt$
  PRINT @(LEN(txt$)*COL_WIDTH%,PROMPTY%) "";:LINE INPUT "", inputStr$
  emptyInputBuffer
  TEXT 0, PROMPTY%, SPACE$(MM.HRES\COL_WIDTH%)
  promptForText$ = inputStr$  
END FUNCTION

'Load file into buffer. Returns number of rows loaded. 0 if failed.
FUNCTION loadFile%(filename$, bIdx%)
  LOCAL row% = 0
  LOCAL lin$

  OPEN filename$ FOR INPUT AS #1

  promptMsg "Loading...", 1

  DO WHILE NOT EOF(#1)
    ON ERROR SKIP 1
      LINE INPUT #1, lin$
    IF MM.ERRNO THEN
      promptMsg "File contains lines with more than 255 characters. Aborting load.", 1
      ON ERROR CLEAR
      loadFile% = 0
      CLOSE #1
      EXIT FUNCTION
    ENDIF
    
    IF NOT wrBufLine%(bIdx%, row%, lin$) THEN
      loadFile% = 0
      CLOSE #1
      EXIT FUNCTION
    ENDIF
    INC row%
  LOOP

  CLOSE #1
  promptMsg "", 0
  bufIsModified%(bIdx%) = 0
  resetUndo 0
  loadFile% = row%
END FUNCTION

'Returns true if OK, false if load aborted
FUNCTION checkAndLoad%(bIdx%, fileToLoad$, confirmIfNew%)
  LOCAL numRows%
  LOCAL fileToLoadl$ = fileToLoad$
  LOCAL fileIsValid% = 0
  LOCAL fileSizel%
  LOCAL ii%

  'Absorb leading and trailing quotes
  IF LEFT$(fileToLoadl$,1) = DOUBLE_QUOTE$ THEN
    fileToLoadl$ = MID$(fileToLoadl$,2)
  ENDIF

  IF RIGHT$(fileToLoadl$,1) = DOUBLE_QUOTE$ THEN
    fileToLoadl$ = LEFT$(fileToLoadl$,LEN(fileToLoadl$)-1)
  ENDIF
  
  IF fileToLoadl$ = "" THEN
    checkAndLoad% = 0
    EXIT FUNCTION
  ENDIF

  fileSizel% = MM.INFO(FILESIZE fileToLoadl$)  

  IF fileSizel% = -1 THEN
    IF confirmIfNew% THEN
      IF UCASE$(promptForAnyKey$("File " + fileToLoadl$ + " not found. Create File? (Y/N)")) <> "Y" THEN
        checkAndLoad% = 0
        EXIT FUNCTION
      ENDIF
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

  setupBuffer bIdx%, numRows%, fileToLoadl$, 0
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
  winRedrawAction%(wIdx%) = NO_REDRAW%
  resetWindow wIdx%
END SUB

SUB resizeWindow(wIdx%, x%, y%, w%, h%)

  'Remove slider before resizing
  LOCAL sliderHeight% = ROW_HEIGHT%*winNumRows%(wIdx%)
  LOCAL sliderTop% = winContentY%(wIdx%)
  LOCAL sliderBottom% = sliderTop%+sliderHeight%
  LOCAL sliderX% = winX%(wIdx%) + winW%(wIdx%) - 2 - SLIDER_WIDTH%
    
  LINE sliderX%, sliderTop%, sliderX%, sliderBottom%-1, SLIDER_WIDTH%, BG_COLOR%

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

SUB drawRowWcolor(x%, y%, linePtr%, wIdx%, bIdx%)
  LOCAL newX%=x%, endX%
  LOCAL startCol% = winBufTopCol%(wIdx%)+1 'Make one based.
  LOCAL endCol% = MIN(winNumCols%(wIdx%)+startCol%, 1+LEN(theStrings$(linePtr%))) 'One based
  LOCAL numCols% = endCol% - startCol%

  IF numCols% > 0 THEN
    LOCAL strToPrint$ = MID$(theStrings$(linePtr%), startCol%, numCols%)

    IF bufSynHLEnabled%(bIdx%) THEN
      parserCSUBCtxt%(PARSER_START_COL%) = startCol%
      parserCSUBCtxt%(PARSER_END_COL%) = endCol%
      parserCSUBCtxt%(PARSER_LINE_X%) = x%
      parserCSUBCtxt%(PARSER_LINE_Y%) = y%
      
      PAGE WRITE 3
      COLOR 0, BG_COLOR%
      TEXT x%, y%, strToPrint$

      PAGE WRITE 4
      
      syntaxHighLight parserCSUBCtxt%(0), theStrings$(linePtr%), KEYWORD_LIST$(0)
      
      newX% = parserCSUBCtxt%(PARSER_LINE_X%)
      IF newX%-x% > 0 THEN
        BLIT x%, y%, x%, y%, newX%-x%, ROW_HEIGHT%, 3, 4 'From page 3 to page 4
        PAGE WRITE 0
        BLIT x%, y%, x%, y%, newX%-x%, ROW_HEIGHT%, 4 'From page 4 to page 0
      ELSE
        PAGE WRITE 0
      ENDIF
      
      COLOR FG_COLOR%, BG_COLOR%
    ELSE
      TEXT x%, y%, strToPrint$
      newX% = x%+LEN(strToPrint$)*COL_WIDTH%
    ENDIF
  ENDIF

  endX% = winContentX%(wIdx%) + winNumCols%(wIdx%)*COL_WIDTH%
  IF endX% > newX% THEN 
    BOX newX%, y%, endX% - newX%, ROW_HEIGHT%, 0, FG_COLOR%, BG_COLOR% 
  ENDIF   
END SUB

'Draw one text row in the given window at the given position.
'checkOtherWin specified to check if the other window needs a
'redraw too (it may be looking at the same text).
SUB drawWinRow(wIdx%, winRow%, checkOtherWin%)
  LOCAL bIdx% = winBuf%(wIdx%)
  LOCAL bufRow% = winBufTopRow%(wIdx%) + winRow%
  LOCAL linePtr% = bufLinePtrs%(bufRow%, bIdx%)
  LOCAL x% = winColToXpos%(wIdx%, 0)
  LOCAL y% = winRowToYpos%(wIdx%, winRow%)
  LOCAL selStartRow%, selStartCol%, selEndRow%, selEndCol%

  selectionBoundaries(wIdx%, selStartRow%, selStartCol%, selEndRow%, selEndCol%)

  'Is a selection active on this line?                         
  IF (bufRow% >= selStartRow%) AND (bufRow% <= selEndRow%) THEN 'Selection active on this line
    LOCAL strToPrint$ = MID$(theStrings$(linePtr%), winBufTopCol%(wIdx%)+1, winNumCols%(wIdx%))

    drawRowWcolor x%, y%, linePtr%, wIdx%, bIdx%

    'Four cases:
    IF (selStartRow% = selEndRow%) THEN '1. Selection starts and ends on current line
      selStartCol% = bufToWinCol%(wIdx%, selStartCol%)
      selEndCol% = bufToWinCol%(wIdx%, selEndCol%)
      IF selEndCol% > selStartCol% THEN                                           
        TEXT winColToXpos%(wIdx%,selStartCol%), y%, MID$(strToPrint$, selStartCol%+1, selEndCol% - selStartCol%),,,, FG_COLOR%, BG_COLOR2%
      ENDIF
    ELSEIF (bufRow% = selStartRow%) THEN '2. Selection starts on current line, ends on a different line.
      selStartCol% = bufToWinCol%(wIdx%, selStartCol%)
      selEndCol% = winNumCols%(wIdx%)
      IF selEndCol% > selStartCol% THEN
        TEXT winColToXpos%(wIdx%,selStartCol%), y%, MID$(strToPrint$, selStartCol%+1, selEndCol% - selStartCol%),,,, FG_COLOR%, BG_COLOR2% 
      ENDIF
    ELSEIF (bufRow% = selEndRow%) THEN '3. Selection started on a different line, ends on current line.
      selStartCol% = 0
      selEndCol% = bufToWinCol%(wIdx%, selEndCol%)
      IF selEndCol% > selStartCol% THEN 
        TEXT winColToXpos%(wIdx%,selStartCol%), y%, MID$(strToPrint$, selStartCol%+1, selEndCol% - selStartCol%),,,, FG_COLOR%, BG_COLOR2%
      ENDIF
      COLOR FG_COLOR%, BG_COLOR%
    ELSE '4. Selection starts and ends on a different line.
      TEXT x%, y%, strToPrint$,,,, FG_COLOR%, BG_COLOR2% 
    ENDIF
  ELSE 'No selection active.
    drawRowWcolor x%, y%, linePtr%, wIdx%, bIdx%
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

'Renders one column of text, used when scrolling horizontally.
SUB drawColWcolor(wIdx%, wCol%)
  LOCAL bIdx% = winBuf%(wIdx%)
  LOCAL bufTopRow% = winBufTopRow%(wIdx%)
  LOCAL bufCol% = winBufTopCol%(wIdx%)+wCol%
  LOCAL numRows% = winNumRows%(wIdx%)
  LOCAL x% = winColToXpos%(wIdx%, wCol%)
  LOCAL y% = winRowToYpos%(wIdx%, 0)
  LOCAL colTxt$ = "", c$
  LOCAL ii% = 0, linePtr%
  
  IF bufSynHLEnabled%(bIdx%) THEN
    PAGE WRITE 4 'syntax highlighting color boxes go onto page 4.
    DO WHILE ii% < numRows%
      linePtr% = bufLinePtrs%(bufTopRow%+ii%, bIdx%)
      c$ = MID$(theStrings$(linePtr%), 1+bufCol%, 1)
      IF c$="" THEN
        c$= " "
      ELSE
        'Set these up each time before calling syntaxHighLight
        parserCSUBCtxt%(PARSER_START_COL%) = bufCol%+1 'Make one based
        parserCSUBCtxt%(PARSER_END_COL%) = bufCol%+2 'Make one based
        parserCSUBCtxt%(PARSER_LINE_X%) = x%
        parserCSUBCtxt%(PARSER_LINE_Y%) = y% + ii%*ROW_HEIGHT%

        syntaxHighLight parserCSUBCtxt%(0), theStrings$(linePtr%), KEYWORD_LIST$(0)
      ENDIF  
      colTxt$ = colTxt$ + c$
      ii% = ii% + 1
    LOOP
    
    PAGE WRITE 3 'Text goes onto page 3.
    COLOR 0, BG_COLOR%
    TEXT x%, y%, colTxt$, "LTV" 'Vertical TEXT
    
    PAGE WRITE 4
    BLIT x%, y%, x%, y%, COL_WIDTH%, ROW_HEIGHT%*numRows%, 3, 4 'From page 3 to page 4
    PAGE WRITE 0
    BLIT x%, y%, x%, y%, COL_WIDTH%, ROW_HEIGHT%*numRows%, 4 'From page 4 to page 0
  ELSE
    DO WHILE ii% < numRows%
      linePtr% = bufLinePtrs%(bufTopRow%+ii%, bIdx%)
      c$ = MID$(theStrings$(linePtr%), 1+bufCol%, 1)
      IF c$="" THEN
        c$= " "
      ENDIF  
      colTxt$ = colTxt$ + c$
      ii% = ii% + 1
    LOOP
    
    TEXT x%, y%, colTxt$, "LTV" 'Vertical TEXT
  ENDIF
END SUB

SUB winContentsScrollUp(wIdx%, startRow%, endRow%)
  LOCAL savedCrsrState%
  LOCAL wX% = winContentX%(wIdx%), wY% = winContentY%(wIdx%)
  LOCAL numRows% = winNumRows%(wIdx%)
   
  IF wIdx% = crsrActiveWidx% THEN
    savedCrsrState% = crsrDisable%()
  ENDIF

  IF startRow% < endRow% THEN
    BLIT wX%, wY%+(1+startRow%)*ROW_HEIGHT%, wX%, wY%+startRow%*ROW_HEIGHT, winNumCols%(wIdx%)*COL_WIDTH%, (endRow%-startRow%)*ROW_HEIGHT%
  ENDIF
  drawWinRow wIdx%, endRow%, 0
  
  IF wIdx% = crsrActiveWidx% THEN
    crsrRestore savedCrsrState%
  ENDIF
END SUB

SUB winContentsScrollDown(wIdx%, startRow%)
  LOCAL savedCrsrState%
  LOCAL wX% = winContentX%(wIdx%), wY% = winContentY%(wIdx%)
  LOCAL numRows% = winNumRows%(wIdx%)
  IF wIdx% = crsrActiveWidx% THEN
    savedCrsrState% = crsrDisable%()
  ENDIF

  IF startRow% < numRows%-1 THEN
    BLIT wX%, wY%+startRow%*ROW_HEIGHT%, wX%, wY%+(startRow%+1)*ROW_HEIGHT%, winNumCols%(wIdx%)*COL_WIDTH%, (numRows%-startRow%-1)*ROW_HEIGHT%
  ENDIF
  drawWinRow wIdx%, startRow%, 0

  IF wIdx% = crsrActiveWidx% THEN
    crsrRestore savedCrsrState%
  ENDIF  
END SUB

SUB winContentsScrollLeft(wIdx%)
  LOCAL savedCrsrState%
  LOCAL wX% = winContentX%(wIdx%), wY% = winContentY%(wIdx%)
  LOCAL winRow%, col%=winNumCols%(wIdx%)-1 'col% is the column where the scrolled-in text should go.
  
  IF wIdx% = crsrActiveWidx% THEN
    savedCrsrState% = crsrDisable%()
  ENDIF

  BLIT wX%+COL_WIDTH%, wY%, wX%, wY%, col%*COL_WIDTH%, winNumRows%(wIdx%)*ROW_HEIGHT%

  drawColWcolor(wIdx%, col%)

  IF wIdx% = crsrActiveWidx% THEN
    crsrRestore savedCrsrState%
  ENDIF  
END SUB

SUB winContentsScrollRight(wIdx%)
  LOCAL savedCrsrState%
  LOCAL wX% = winContentX%(wIdx%), wY% = winContentY%(wIdx%)
  LOCAL winRow%, col%=0 'col% is the column where the scrolled-in text should go.
  
  IF wIdx% = crsrActiveWidx% THEN
    savedCrsrState% = crsrDisable%()
  ENDIF

  BLIT wX%, wY%, wX%+COL_WIDTH%, wY%, (winNumCols%(wIdx%)-1)*COL_WIDTH%, winNumRows%(wIdx%)*ROW_HEIGHT%

  drawColWcolor(wIdx%, col%)

  IF wIdx% = crsrActiveWidx% THEN
    crsrRestore savedCrsrState%
  ENDIF  
END SUB

'Draw all text rows in given window.
SUB drawWinContents(wIdx%)
  LOCAL winRow%
  LOCAL savedCrsrState%
  LOCAL bIdx% = winBuf%(wIdx%)

  IF wIdx% = crsrActiveWidx% THEN
    savedCrsrState% = crsrDisable%()
  ENDIF

  IF bufIsConsole%(bIdx%) THEN
    LOCAL x1% = winBufTopCol%(wIdx%)*COL_WIDTH%
    LOCAL y1% = winBufTopRow%(wIdx%)*ROW_HEIGHT%
    LOCAL x2% = winContentX%(wIdx%)
    LOCAL y2% = winContentY%(wIdx%)
    LOCAL w% = winNumCols%(wIdx%)*COL_WIDTH%
    LOCAL h% = winNumRows%(wIdx%)*ROW_HEIGHT%
    
    BLIT x1%, y1%, x2%, y2%, w%, h%, 2
  ELSE
    FOR winRow%=0 TO winNumRows%(wIdx%) - 1
      drawWinRow wIdx%, winRow%, 0
    NEXT winRow%
  ENDIF
  
  IF wIdx% = crsrActiveWidx% THEN
    crsrRestore savedCrsrState%
  ENDIF
END SUB

SUB drawSlider(wIdx%)
  STATIC prev_bIdx%(1) = (-1, -1)
  STATIC prev_yFrom%(1) = (-1, -1)
  STATIC prev_yTo%(1) = (-1, -1)
  STATIC prev_x%(1) = (-1, -1)
  STATIC prev_yTop%(1) = (-1, -1)
  STATIC prev_yBottom%(1) = (-1, -1)

  LOCAL bIdx% = winBuf%(wIdx%)
  
  IF bufNumRows%(bIdx%) = 0 THEN
    EXIT SUB
  ENDIF
  
  LOCAL wHeight% = ROW_HEIGHT%*winNumRows%(wIdx%)
  LOCAL yTop% = winContentY%(wIdx%)
  LOCAL yBottom% = yTop%+wHeight%
  LOCAL yFrom% = yTop% + INT(wHeight%*MIN(winBufTopRow%(wIdx%)/bufNumRows%(bIdx%),1))
  LOCAL yTo% = yTop% + INT(wHeight%*MIN((winBufTopRow%(wIdx%)+winNumRows%(wIdx%))/bufNumRows%(bIdx%), 1))
  LOCAL x% = winX%(wIdx%) + winW%(wIdx%) - 2 - SLIDER_WIDTH%
  
  IF yTo%-1 <= yFrom% THEN
    yTo% = yTo%+1
  ENDIF
  
  IF (prev_bIdx%(wIdx%) = bIdx%) AND (prev_yFrom%(wIdx%) = yFrom%) AND (prev_yTo%(wIdx%) = yTo%) AND (prev_x%(wIdx%) = x%) AND (prev_yTop%(wIdx%) = yTop%) AND (prev_yBottom%(wIdx%) = yBottom%) THEN
    EXIT SUB
  ENDIF
  
  LINE x%, yTop%, x%, yBottom%-1, SLIDER_WIDTH%, BG_COLOR%
  LINE x%, yFrom%, x%, yTo%-1, SLIDER_WIDTH%, FG_COLOR2%
  
  prev_bIdx%(wIdx%) = bIdx%
  prev_yFrom%(wIdx%) = yFrom%
  prev_yTo%(wIdx%) = yTo%
  prev_x%(wIdx%) = x%
  prev_yTop%(wIdx%) = yTop%
  prev_yBottom%(wIdx%) = yBottom%  
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

  'Print inverted.
  TEXT headerX%, headerY%, headerLeft$ + headerCenter$ + SPACE$(numSpaces%) + headerRight$,,,, BG_COLOR%, FG_COLOR2%
END SUB

SUB drawGenFooter
  LOCAL footerX% = 0
  LOCAL footerY% = MM.VRES - ROW_HEIGHT% + 1
  LOCAL footerW% = FULL_SCREEN_WINDOW_W%
  LOCAL insOvrModeString$, compatModeString$

  IF crsrMode% = CRSR_MODE_INS% THEN
    insOvrModeString$ = " (INS) "
  ELSE
    insOvrModeString$ = " (OVR) "
  ENDIF

  IF SERIAL_INPUT_COMPAT_MODE% THEN
    compatModeString$ = "(Serial Compat)"
  ELSE
    compatModeString$ = "               "  
  ENDIF
  
  LOCAL footerLeft$ = "XEdit V"+VERSION$+" by Epsilon. " + insOvrModeString$ + compatModeString$
  LOCAL footerRight$ = "(CWD: "+CWD$+")  F1 = Help  "
  
  'Print inverted.
  TEXT footerX%, footerY%, footerLeft$ + SPACE$(footerW%\COL_WIDTH% - LEN(footerLeft$) - LEN(footerRight$)) + footerRight$,,,,BG_COLOR%, FG_COLOR%
END SUB

'Scroll horizontally to give offset from the left.
SUB scrollHoffset(wIdx%, fromLeft%)
  IF winBufTopCol%(wIdx%) <> fromLeft% THEN
    winBufTopCol%(wIdx%) = fromLeft%
    winRedrawAction%(wIdx%) = FULL_REDRAW%
  ENDIF
END SUB

'Scroll horizontally a number of columns. Positive numCols% is scroll left.
SUB scrollHdelta(wIdx%, numCols%)
  IF numCols%=0 THEN
    EXIT SUB
  ENDIF
  
  winBufTopCol%(wIdx%) = winBufTopCol%(wIdx%) + numCols%
  
  SELECT CASE numCols%
    CASE 1
      winRedrawAction%(wIdx%) = SCROLL_LEFT%
    CASE -1
      winRedrawAction%(wIdx%) = SCROLL_RIGHT%
    CASE ELSE
      winRedrawAction%(wIdx%) = FULL_REDRAW%
  END SELECT
END SUB

'Scroll vertically a number of rows. Positive numRows% is scroll down.
SUB scrollVdelta(wIdx%, numRows%)
  LOCAL bIdx% = winBuf%(widx%)
  winBufTopRow%(wIdx%) = winBufTopRow%(wIdx%) + numRows%
  IF numRows% = -1 THEN
    winContentScrollDownStartRow% = 0
    winRedrawAction%(wIdx%) = SCROLL_DOWN%
  ELSEIF numRows% = 1 THEN
    winContentScrollUpStartRow% = 0
    winContentScrollUpEndRow% = winNumRows%(wIdx%) - 1
    winRedrawAction%(wIdx%) = SCROLL_UP%
  ELSE
    winRedrawAction%(wIdx%) = FULL_REDRAW%
  ENDIF
END SUB

'--> Key Handler Section:
'------------------------

'Exit key handler.
SUB requestExit execFile$
  LOCAL bIdx%
  LOCAL anyFileModified% = 0

  FOR bIdx%=0 TO 1
    IF bufIsModified%(bIdx%) THEN
      anyFileModified% = 1
    ENDIF
  NEXT bIdx%

  IF anyFileModified% AND NOT DISABLE_CONFIRMATION_PROMPTS% THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Are you sure you want to quit? (Y/N)")
    exitRequested% = (UCASE$(yesNo$)="Y")
  ELSE
    exitRequested% = 1
  ENDIF
  
  IF exitRequested% THEN
    executeOnExit$ = execFile$
  ENDIF
END SUB

SUB exitKeyHandler
  requestExit ""
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
      
      LOCAL leftw%
      'Active window should go where cursor is
      IF winWinCrsrCol%(crsrActiveWidx%) < winNumCols%(crsrActiveWidx%)\2 THEN
        leftw% = crsrActiveWidx% 'Cursor is in left half
      ELSE
        leftw% = NOT crsrActiveWidx% 'Cursor is in right half
      ENDIF
      
      resizeWindow leftw%, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%\2, FULL_SCREEN_WINDOW_H%
      resizeWindow NOT leftw%, FULL_SCREEN_WINDOW_W%\2, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%\2, FULL_SCREEN_WINDOW_H%
      
      gotoBufPos winBufCrsrRow%(crsrActiveWidx%), winBufCrsrCol%(crsrActiveWidx%), 0, 1
      
      'Temp switch to other window to restore cursor position.
      crsrActiveWidx% = NOT crsrActiveWidx%
      restoreBufPos
      crsrActiveWidx% = NOT crsrActiveWidx%
      
      drawWindow 0: drawWindow 1

    CASE HSPLIT%
      crsrOff
      
      LOCAL topw%
      'Active window should go where cursor is
      IF winWinCrsrRow%(crsrActiveWidx%) < winNumRows%(crsrActiveWidx%)\2 THEN
        topw% = crsrActiveWidx% 'Cursor is in top half
      ELSE
        topw% = NOT crsrActiveWidx% 'Cursor is in bottom half
      ENDIF

      resizeWindow topw%, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y%, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%\2
      resizeWindow NOT topw%, FULL_SCREEN_WINDOW_X%, FULL_SCREEN_WINDOW_Y% + FULL_SCREEN_WINDOW_H%\2, FULL_SCREEN_WINDOW_W%, FULL_SCREEN_WINDOW_H%\2

      gotoBufPos winBufCrsrRow%(crsrActiveWidx%), winBufCrsrCol%(crsrActiveWidx%), 0, 1

      'Temp switch to other window to restore cursor position.
      crsrActiveWidx% = NOT crsrActiveWidx%
      restoreBufPos
      crsrActiveWidx% = NOT crsrActiveWidx%
      
      drawWindow 0: drawWindow 1
  END SELECT
END SUB

SUB toggleActiveWindowKeyHandler
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
  bufSavedTopCol%(bIdx%) = winBufTopCol%(crsrActiveWidx%)
  bufSavedTopRow%(bIdx%) = winBufTopRow%(crsrActiveWidx%)

  bIdx% = NOT bIdx%
  winBuf%(crsrActiveWidx%) = bIdx%

  'Restore other buffer's cursor position
  restoreBufPos
END SUB

SUB loadIntoCurrentBufKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  
  IF bufIsModified%(bIdx%) AND NOT DISABLE_CONFIRMATION_PROMPTS% THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Discard the changes? (Y/N)")
    IF UCASE$(yesNo$) <> "Y" THEN
      EXIT SUB
     ENDIF
  ENDIF

  LOCAL filename$
  LOCAL promptForNewFile%
  
  IF ENABLE_FILE_DIALOG_BOX% THEN
    filename$ = GetFileName(15,"*") 
    promptForNewFile% = 0
  ELSE
    filename$ = promptForText$("Load File: ")
    promptForNewFile% = NOT DISABLE_CONFIRMATION_PROMPTS%
  ENDIF
  
  IF checkAndLoad%(winBuf%(crsrActiveWidx%), filename$, promptForNewFile%) THEN
    resetWindow crsrActiveWidx%
    winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    IF winBuf%(0) = winBuf%(1) THEN 'If the other window looks into the same buffer, reset that one too.
      resetWindow NOT crsrActiveWidx%
      winRedrawAction%(NOT crsrActiveWidx%) = FULL_REDRAW%
    ENDIF
  ENDIF
END SUB

FUNCTION closeBuffer%()
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL ii%
  
  IF bufIsModified%(bIdx%) AND NOT DISABLE_CONFIRMATION_PROMPTS% THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Discard the changes? (Y/N)")
    IF UCASE$(yesNo$) <> "Y" THEN
      closeBuffer% = 0
      EXIT FUNCTION
     ENDIF
  ENDIF
  
  'A new, empty buffer
  FOR ii%=0 TO bufNumRows%(bIdx%)-1
    freeLine bufLinePtrs%(ii%, bIdx%)
  NEXT ii%

  setupBuffer bIdx%, 0, "", 0
  
  resetWindow crsrActiveWidx%
  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
  IF winBuf%(0) = winBuf%(1) THEN 'If the other window looks into the same buffer, reset that one too.
    resetWindow NOT crsrActiveWidx%
    winRedrawAction%(NOT crsrActiveWidx%) = FULL_REDRAW%
  ENDIF
   
  closeBuffer% = 1
END FUNCTION

SUB closeBufferKeyHandler
  IF closeBuffer%() THEN
    promptMsg "Buffer closed.", 1
  ENDIF
END SUB

SUB crsrRightKeyHandler
  crsrRightImp 1 'with drawing
END SUB

SUB crsrRight num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    crsrRightImp 0 'without drawing
    ii%=ii%+1
  LOOP
END SUB

SUB crsrRightImp(draw%) 'Set draw% to 0 to skip drawing.
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

  IF draw% AND selectMode%(crsrActiveWidx%) THEN
    crsrOff

    IF winRedrawAction%(crsrActiveWidx%) <> NO_REDRAW%  THEN
      winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    ELSE
      drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
      drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
    ENDIF
  ENDIF
END SUB

SUB crsrLeft num%
  LOCAL ii%=0
  DO WHILE ii%<num%
    crsrLeftImp 0 'without drawing
    ii%=ii%+1
  LOOP
END SUB

SUB crsrLeftImp draw% 'Set to 0 to skip drawing
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
  
  IF draw% AND selectMode%(crsrActiveWidx%) THEN
    crsrOff

    IF winRedrawAction%(crsrActiveWidx%) <> NO_REDRAW%  THEN
      winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    ELSE
      drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
      drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1    
    ENDIF
  ENDIF
END SUB

SUB crsrLeftKeyHandler
  crsrLeftImp 1
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
    EXIT SUB
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

  'Figure out if we should scroll vertically
  LOCAL newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)
  IF (newBufCrsrRow% > oldBufCrsrRow%) AND (oldWinCrsrRow% = newWinCrsrRow%) THEN
    scrollVdelta crsrActiveWidx%, 1
  ENDIF

  'Figure out if we should scroll horizontally
  IF newBufCrsrCol% <> oldBufCrsrCol% THEN
    LOCAL newWinCrsrCol% = MIN(newBufCrsrCol%, winNumCols%(crsrActiveWidx%)-1)  
    winWinCrsrCol%(crsrActiveWidx%) = newWinCrsrCol%
    scrollHoffset crsrActiveWidx%, newBufCrsrCol% - newWinCrsrCol%
  ENDIF
  
  IF selectMode%(crsrActiveWidx%) THEN
    SELECT CASE winRedrawAction%(crsrActiveWidx%)
      CASE SCROLL_UP%
        winContentScrollUpEndRow% = winNumRows%(crsrActiveWidx%)-2
      CASE SCROLL_DOWN%
        winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    END SELECT

    IF winRedrawAction%(crsrActiveWidx%) <> FULL_REDRAW% THEN  
      crsrOff
      drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
      drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
    ENDIF
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

  'Figure out if we should scroll vertically.
  LOCAL newWinCrsrRow% = winWinCrsrRow%(crsrActiveWidx%)

  IF (newBufCrsrRow% < oldBufCrsrRow%) AND (oldWinCrsrRow% = newWinCrsrRow%) THEN
    scrollVdelta crsrActiveWidx%, -1
  ENDIF 

  'Figure out if we should scroll horizontally.
  IF newBufCrsrCol% <> oldBufCrsrCol% THEN
    LOCAL newWinCrsrCol% = MIN(newBufCrsrCol%, winNumCols%(crsrActiveWidx%)-1)
    winWinCrsrCol%(crsrActiveWidx%) = newWinCrsrCol%
    scrollHoffset crsrActiveWidx%, newBufCrsrCol% - newWinCrsrCol%
  ENDIF
  
  IF selectMode%(crsrActiveWidx%) THEN
    SELECT CASE winRedrawAction%(crsrActiveWidx%)
      CASE SCROLL_UP%
        winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
      CASE SCROLL_DOWN%
        winContentScrollDownStartRow% = 1
    END SELECT
  
    IF winRedrawAction%(crsrActiveWidx%) <> FULL_REDRAW% THEN
      crsrOff
      drawWinRow crsrActiveWidx%, oldWinCrsrRow%, 1
      drawWinRow crsrActiveWidx%, newWinCrsrRow%, 1
    ENDIF
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
    winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
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
    winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
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
  
  'Note that typically we register for undo before applying the action but here
  'we do it after.
  winSelectCol%(crsrActiveWidx%) = startCol%
  winSelectRow%(crsrActiveWidx%) = startRow%
  registerForUndo UNDO_PASTE% 'Reusing the paster undo action.
  winSelectCol%(crsrActiveWidx%) = -1
  winSelectRow%(crsrActiveWidx%) = -1

  IF (winRedrawAction%(crsrActiveWidx%) <> FULL_REDRAW%) AND (winRedrawAction%(crsrActiveWidx%) <> SCROLL_UP%) THEN
    crsrOff
    drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%)-1, 0 
    winContentScrollDownStartRow% = winWinCrsrRow%(crsrActiveWidx%)
    winRedrawAction%(crsrActiveWidx%) = SCROLL_DOWN%
  ENDIF
  
  IF winBuf%(0) = winBuf%(1) THEN 
    IF winBufTopRow%(NOT crsrActiveWidx%) > winBufCrsrRow%(crsrActiveWidx%) THEN
      'Looking at same buffer further down out of view -> just increase cursor position.
      winBufTopRow%(NOT crsrActiveWidx%) = winBufTopRow%(NOT crsrActiveWidx%) + 1
      winBufCrsrRow%(NOT crsrActiveWidx%) = winBufCrsrRow%(NOT crsrActiveWidx%) + 1
    ELSEIF winBufTopRow%(NOT crsrActiveWidx%) + winNumRows%(NOT crsrActiveWidx%) < winBufCrsrRow%(crsrActiveWidx%) THEN
      'Out of view further up -> nothing to do
      EXIT SUB
    ELSE    
      'Other window looks into the same buffer, redraw that one too.
      winRedrawAction%(NOT crsrActiveWidx%) = FULL_REDRAW%
    ENDIF
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
      IF winRedrawAction%(crsrActiveWidx%) <> FULL_REDRAW% THEN
        crsrOff
        drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 0 
        winContentScrollUpStartRow% = winWinCrsrRow%(crsrActiveWidx%)+1
        winContentScrollUpEndRow% = winNumRows%(crsrActiveWidx%)-1
        winRedrawAction%(crsrActiveWidx%) = SCROLL_UP%
      ENDIF

      IF winBuf%(0) = winBuf%(1) THEN 
        IF winBufTopRow%(NOT crsrActiveWidx%) > winBufCrsrRow%(crsrActiveWidx%) THEN
          'Looking at same buffer further down out of view -> just decrease cursor position.
          winBufTopRow%(NOT crsrActiveWidx%) = winBufTopRow%(NOT crsrActiveWidx%) - 1
          winBufCrsrRow%(NOT crsrActiveWidx%) = winBufCrsrRow%(NOT crsrActiveWidx%) - 1
        ELSEIF winBufTopRow%(NOT crsrActiveWidx%) + winNumRows%(NOT crsrActiveWidx%) < winBufCrsrRow%(crsrActiveWidx%) THEN
          'Out of view further up -> nothing to do
          delete% = 1
          EXIT FUNCTION
        ELSE    
          'Other window looks into the same buffer, redraw that one too.
          winRedrawAction%(NOT crsrActiveWidx%) = FULL_REDRAW%
        ENDIF
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
    winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    LOCAL oWidx% = NOT crsrActiveWidx%
    IF winBuf%(0) = winBuf%(1) THEN 'If the other window looks into the same buffer    
      IF winBufTopRow%(oWidx%) > winBufCrsrRow%(crsrActiveWidx%) THEN
        'Looking at same buffer further down out of view -> just adjust cursor position.
        winBufTopRow%(oWidx%) = winBufTopRow%(oWidx%) + numRows% - 1
        winBufCrsrRow%(oWidx%) = winBufCrsrRow%(oWidx%) + numRows% - 1
      ELSEIF winBufTopRow%(oWidx%) + winNumRows%(oWidx%) < fromRow% THEN
        'Out of view further up -> nothing to do
        EXIT SUB
      ELSE    
        'Other window looks into the same buffer, redraw that one too.
        winRedrawAction%(oWidx%) = FULL_REDRAW%
      ENDIF
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
  
  IF linLen% - toCol% - 1 < 0 THEN
    Error "Invalid num. characters: "+STR$(linLen%)+" "+STR$(toCol%)
  ENDIF
  
  LOCAL r$ = RIGHT$(toLin$, linLen% - toCol% - 1)
  
  IF LEN(l$) + LEN(r$) > 255 THEN
    promptMsg "Can't delete selection. Would exceed max. line length.", 1
    deleteSelection% = 0
    EXIT FUNCTION
  ENDIF

  ok% = wrBufLine%(bIdx%, fromRow%, l$ + r$)
  deleteBufLines(bIdx%, fromRow%+1, toRow% - fromRow%)
  
  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
  IF winBuf%(0) = winBuf%(1) THEN 
    IF winBufTopRow%(NOT crsrActiveWidx%) > toRow% THEN
      'Looking at same buffer further down out of view -> just decrease cursor position.
      winBufTopRow%(NOT crsrActiveWidx%) = winBufTopRow%(NOT crsrActiveWidx%) - toRow% + fromRow%
      winBufCrsrRow%(NOT crsrActiveWidx%) = winBufCrsrRow%(NOT crsrActiveWidx%) - toRow% + fromRow%
    ELSEIF winBufTopRow%(NOT crsrActiveWidx%) + winNumRows%(NOT crsrActiveWidx%) < fromRow% THEN
      'Out of view further up -> nothing to do
      deleteSelection% = 1
      EXIT FUNCTION
    ELSE    
      'Other window looks into the same buffer, redraw that one too.
      winRedrawAction%(NOT crsrActiveWidx%) = FULL_REDRAW%
    ENDIF
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
  LOCAL startRow%, startCol%, endRow%, endCol%, ok%
  LOCAL ii%, jj%
  LOCAL screenCtxt%(SCREEN_CONTEXT_SIZE%-1)
  LOCAL lin$
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
    
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

  crsrOff

  jj% = startRow%
  DO WHILE jj% <= endRow%
    rdBufLine(bIdx%, jj%, lin$)

    IF LEN(lin$)+numSpaces% < 255 THEN 'There is room for one more tab
      ok% = wrBufLine%(bIdx%, jj%, SPACE$(numSpaces%) + lin$)
    ENDIF

    jj% = jj%+1
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
    promptMsg "Indenting...", 1
    registerForUndo UNDO_INDENT%
    indentSelection
    promptMsg "Done.", 1
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
  LOCAL ii%, jj%, nn%, ok%
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
    rdBufLine(bIdx%, jj%, lin$)
    
    'Only consume spaces, and as much as possible spaces up to numSpaces.
    ii%=0:nn%=0
    DO WHILE ii% < numSpaces%
      IF MID$(lin$, 1+ii%, 1) = " " THEN
        nn% = nn%+1
      ENDIF
      ii%= ii%+1
    LOOP
    
    IF nn% THEN
      ok%=wrBufLine%(bIdx%, jj%, MID$(lin$, 1+nn%))
    ENDIF
    
    jj% = jj%+1
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
    promptMsg "Unindenting...", 1

    registerForUndo UNDO_UNINDENT%
    unindentSelection
    promptMsg "Done.", 1
  ELSE
    unindent
  ENDIF
END SUB

SUB cutKeyHandler
  IF NOT selectMode%(crsrActiveWidx%) THEN
    EXIT SUB
  ENDIF

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
  IF NOT selectMode%(crsrActiveWidx%) THEN
    EXIT SUB
  ENDIF

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

  'Note that typically we register for undo before applying the action but here
  'we do it after.
  winSelectCol%(crsrActiveWidx%) = startCol%
  winSelectRow%(crsrActiveWidx%) = startRow%
  registerForUndo UNDO_PASTE%
  winSelectCol%(crsrActiveWidx%) = -1
  winSelectRow%(crsrActiveWidx%) = -1

  IF (numRows%=1) AND (winRedrawAction%(crsrActiveWidx%) = NO_REDRAW%) THEN
    drawWinRow crsrActiveWidx%, winWinCrsrRow%(crsrActiveWidx%), 1
  ELSE
    LOCAL oWidx% = NOT crsrActiveWidx%
    winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
        
    IF winVisible%(oWidx%) AND (winBuf%(0) = winBuf%(1)) THEN 
      IF winBufTopRow%(oWidx%) > winBufCrsrRow%(crsrActiveWidx%) THEN
        'Looking at same buffer further down out of view -> just adjust cursor position.
        winBufTopRow%(oWidx%) = winBufTopRow%(oWidx%) + numRows% - 1
        winBufCrsrRow%(oWidx%) = winBufCrsrRow%(oWidx%) + numRows% - 1
      ELSEIF winBufTopRow%(oWidx%) + winNumRows%(oWidx%) < startRow% THEN
        'Out of view further up -> nothing to do
      ELSE    
        'Other window looks into the same buffer, redraw that one too.
        winRedrawAction%(oWidx%) = FULL_REDRAW%
      ENDIF
    ENDIF
  ENDIF

  promptMsg "Paste done.", 1
END SUB

SUB restoreBufPos
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)

  winBufCrsrRow%(crsrActiveWidx%) = bufSavedCrsrRow%(bIdx%)
  winBufCrsrCol%(crsrActiveWidx%) = bufSavedCrsrCol%(bIdx%)
  
  LOCAL topRowLimit% = winBufCrsrRow%(crsrActiveWidx%)-winNumRows%(crsrActiveWidx%)+1  
  LOCAL topColLimit% = winBufCrsrCol%(crsrActiveWidx%)-winNumCols%(crsrActiveWidx%)+1  
  
  winBufTopRow%(crsrActiveWidx%) = MAX(bufSavedTopRow%(bIdx%), topRowLimit%)
  winBufTopCol%(crsrActiveWidx%) = MAX(bufSavedTopCol%(bIdx%), topColLimit%)

  winWinCrsrRow%(crsrActiveWidx%) = winBufCrsrRow%(crsrActiveWidx%) - winBufTopRow%(crsrActiveWidx%)
  winWinCrsrCol%(crsrActiveWidx%) = winBufCrsrCol%(crsrActiveWidx%) - winBufTopCol%(crsrActiveWidx%)

  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
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
    LOCAL numPagesToScroll% = INT((bufRow% - winBufTopRow%(crsrActiveWidx%))/winNumRows%(crsrActiveWidx%))
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

  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
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
    findKeyHandler 1
  ELSE
    findStrToFind 1, 1 'Keep searching for the same string.
  ENDIF
END SUB

SUB findPrevKeyHandler
  IF strToFind$="" THEN
    findKeyHandler -1
  ELSE
    findStrToFind -1, 1 'Keep searching for the same string.
  ENDIF
END SUB

SUB findKeyHandler(direction%)
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
    IF direction%=-1 THEN
      strToFind$ = promptForText$("Reverse Find String: ")
    ELSE
      strToFind$ = promptForText$("Find String: ")
    ENDIF
  ENDIF

  IF strToFind$ = "" THEN
    EXIT SUB
  ENDIF

  IF direction%=-1 THEN
    findStrToFind direction%, 1
  ELSE
    findStrToFind direction%, 0
  ENDIF  
END SUB
  
SUB findStrToFind(direction%, skipCurrent%)
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL row%, col%, linLen%, firstPass%=1
  LOCAL lin$
  LOCAL key$
  LOCAL strToFindAbbr$
  LOCAL strToFindLen% = LEN(strToFind$)
  LOCAL dirStr$
  CONST STR_TO_FIND_ABBR_MAX% = 40
  
  IF LEN(strToFind$) > STR_TO_FIND_ABBR_MAX%) THEN
    strToFindAbbr$ = DOUBLE_QUOTE$+LEFT$(strToFind, STR_TO_FIND_ABBR_MAX%)+"..."+DOUBLE_QUOTE$
  ELSE
    strToFindAbbr$ = DOUBLE_QUOTE+strToFind$+DOUBLE_QUOTE$
  ENDIF

  IF direction%=1 THEN
    dirStr$ = "Forward"
  ELSE
    dirStr$ = "Reverse"
  ENDIF
  
  promptMsg dirStr$ + " searching for "+strToFindAbbr$+"...", 1

  IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
    strToFind$=UCASE$(strToFind$)
  ENDIF
  
  row%=winBufCrsrRow%(crsrActiveWidx%)

  DO WHILE 1
    DO WHILE (row%>=0) AND (row% < bufNumRows%(bIdx%))
      'Inlined rdBufLine
      lin$ = theStrings$(bufLinePtrs%(row%, bIdx%))
      linLen% = LEN(lin$)
      
      IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
        lin$=UCASE$(lin$)
      ENDIF
      
      'If there are no matches on this line, move on to next/prev row. Else scan through hits.
      IF INSTR(lin$, strToFind$) THEN
        IF firstPass% THEN
          col% = winBufCrsrCol%(crsrActiveWidx%) + direction%*skipCurrent%
          firstPass%=0
        ELSE
          IF direction% = 1 THEN
            col% = 0
          ELSE
            col% = linLen%-strToFindLen%
          ENDIF
        ENDIF

        drawSlider crsrActiveWidx%
        
        'There may be multiple hits on the same row, so we have to loop over columns
        'until the end of the line is reached.
        DO WHILE (col%>=0) AND (col% <= linLen%-strToFindLen%)
          IF INSTR(1+col%, lin$, strToFind$) = 1+col% THEN
            'Go to rightmost character of match to make sure it doesn't fall off the screen.
            gotoBufPos row%, col%+LEN(strToFind$)-1, 0, 1
            'Now paddle back to the first character of the match.
            crsrLeft LEN(strToFind$)-1
            
            winSelectCol%(crsrActiveWidx%) = col% + strToFindLen%
            winSelectRow%(crsrActiveWidx%) = row%
            winBufCrsrCol%(crsrActiveWidx%) = col% 
            winBufCrsrRow%(crsrActiveWidx%) = row%
  
            'We can't just flag a request for redraw here because we're not returning
            'to the mainloop (yet). We block at the prompt.
            drawWinContents crsrActiveWidx%
            drawWinHeader crsrActiveWidx%
  
            key$ = UCASE$(promptForAnyKey$("Find "+strToFindAbbr$+", N=Next/P=Previous/Enter=Done."))
            SELECT CASE key$
              CASE "N", CHR$(14) 'Ctrl-N INKEY value.
                direction% = 1
              CASE "P"
                direction% = -1
              CASE ELSE
                EXIT SUB
            END SELECT

            IF direction%=1 THEN
              dirStr$ = "Forward"
            ELSE
              dirStr$ = "Reverse"
            ENDIF
  
            promptMsg dirStr$ + " searching for "+strToFindAbbr$+"...", 1
          ENDIF
          
          'Remove selection.
          winSelectCol%(crsrActiveWidx%) = -1
          winSelectRow%(crsrActiveWidx%) = -1
  
          col% = col% + direction%
        LOOP
      ENDIF
      
      firstPass%=0 'Reset first pass after first line.
      row% = row% + direction%
    LOOP

    IF row%<0 THEN
      IF UCASE$(promptForAnyKey$("Beginning of buffer reached. Wrap around? (Y/N)")) <> "Y" THEN
        EXIT SUB
      ENDIF
      row% = bufNumRows%(bIdx%)-1
      rdBufLine(bIdx%, row%, lin$)
      col% = LEN(lin$)
    ELSE
      IF UCASE$(promptForAnyKey$("End of buffer reached. Wrap around? (Y/N)")) <> "Y" THEN
        EXIT SUB
      ENDIF
      row% = 0
      col% = 0
    ENDIF
    
    promptMsg dirStr$+" searching for "+strToFindAbbr$+"...", 1
  LOOP
END SUB

SUB findAcrossFiles(bIdx%)
  bufFilename$(bIdx%) = "(xFind)"
  bufSynHLEnabled%(bIdx%) = 0

  IF strToFind$ = "" THEN
    strToFind$ = promptForText$("xFind String: ")
  ENDIF
  
  IF strToFind$ = "" THEN
    EXIT SUB
  ENDIF
  
  LOCAL fspec$ = promptForText$("xFind File Spec (Optional): ")  
  IF fspec$="" THEN
    fspec$="."
  ENDIF
  
  'Redraw window here because xFind can take a while
  drawWinContents crsrActiveWidx%
  drawWinHeader crsrActiveWidx%

  'bufSynHLEnabled%(bIdx%) = 0

  promptMsg "Searching...", 1
  xFind(bIdx%, strToFind$, fspec$)
  promptMsg "Done.", 1
  
  bufIsModified%(bIdx%) = 0
  resetUndo 0  
END SUB

SUB findAcrossFilesKeyHandler
  LOCAL selStartRow%, selStartCol%, selEndRow%, selEndCol%
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  
  strToFind$=""
  
  'If there's a selection, use that as search text. Otherwise prompt for text to search.
  IF selectMode%(crsrActiveWidx%) THEN
    selectionBoundaries(crsrActiveWidx%, selStartRow%, selStartCol%, selEndRow%, selEndCol%)

    IF selStartRow% = selEndRow% THEN
      rdBufLine(bIdx%, selStartRow%, strToFind$)
      strToFind$ = MID$(strToFind$, 1+selStartCol%, selEndCol%-selStartCol%)
    ENDIF
  ENDIF

  IF NOT closeBuffer%() THEN
    EXIT SUB
  ENDIF

  findAcrossFiles bIdx%
END SUB

SUB replaceKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%), firstPass%=1
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
    strToReplaceAbbr$ = DOUBLE_QUOTE$+LEFT$(strToReplace, STR_TO_REPLACE_ABBR_MAX%)+"..."+DOUBLE_QUOTE$
  ELSE
    strToReplaceAbbr$ = DOUBLE_QUOTE$+strToReplace$+DOUBLE_QUOTE$
  ENDIF

  replaceWith$ = promptForText$("Replace with: ")

  IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
    strToReplace$=UCASE$(strToReplace$)
  ENDIF

  LOCAL row%, col%, startRow%, startCol%, linLen%, ok%
  LOCAL linCS$, lin$, l$, r$
  LOCAL key$=""
  LOCAL wrappedAround% = 0

  row%=winBufCrsrRow%(crsrActiveWidx%)
  col%=winBufCrsrCol%(crsrActiveWidx%)
  startRow% = winBufCrsrRow%(crsrActiveWidx%)
  startCol% = winBufCrsrCol%(crsrActiveWidx%)

  promptMsg "Searching for "+strToReplaceAbbr$+"...", 1

  DO WHILE 1
    DO WHILE (row%>=0) AND (row% < bufNumRows%(bIdx%))
      'Inlined rdBufLine
      lin$ = theStrings$(bufLinePtrs%(row%, bIdx%))      
      linLen% = LEN(lin$)

      linCS$ = lin$
      IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
        lin$=UCASE$(lin$)
      ENDIF

      IF firstPass% THEN
        IF selectMode%(crsrActiveWidx%) THEN
          col% = selStartCol%
        ELSE
          col% = winBufCrsrCol%(crsrActiveWidx%)          
        ENDIF
        firstPass%=0
      ELSE
        col% = 0
      ENDIF
      
      'key$="A" when a replace-all is requested.
      'This check tests if replace-all has gone full circle
      'The y/n cases don't use this. They rely on the end/beginning of buffer
      'reached prompts.
      IF (key$="A") AND (row%>=startRow%) AND wrappedAround% THEN
        gotoBufPos startRow%, startCol%, 0, 1
        promptMsg "Replace done.", 1
        EXIT SUB
      ENDIF

      'If there are no matches on this line, move on to next/prev row. Else scan through hits.
      IF INSTR(lin$, strToReplace$) THEN
        drawSlider crsrActiveWidx%
        
        'There may be multiple hits on the same row, so we have to loop over columns
        'until the end of the line is reached.
        DO WHILE (col%>=0) AND (col% < linLen%)
          IF INSTR(1+col%, lin$, strToReplace$) = 1+col% THEN
            gotoBufPos row%, col%+LEN(strToReplace$)-1, 0, 1
            'Now paddle back to the first character of the match.
            crsrLeft LEN(strToReplace$)-1
  
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
  
            'Remove selection
            winSelectCol%(crsrActiveWidx%) = -1
            winSelectRow%(crsrActiveWidx%) = -1
  
            SELECT CASE key$
              CASE "N"
              CASE "Y","A"
                l$ = LEFT$(linCS$, col%)
                r$ = MID$(linCS$, 1+col% + LEN(strToReplace$))
                IF LEN(l$)+LEN(replaceWith$)+LEN(r$) > 255 THEN
                  promptMsg "Can't replace. Would exceed max. line length.", 1
                  EXIT SUB
                ENDIF
                linCS$ = l$ + replaceWith$ + r$
                ok% = wrBufLine%(bIdx%, row%, linCS$)
                
                IF NOT SEARCH_IS_CASE_SENSITIVE% THEN
                  lin$=UCASE$(linCS$)
                ELSE
                  lin$=linCS$
                ENDIF
                
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
      ENDIF
 
      firstPass%=0 'Reset first pass after first line.      
      row% = row% + 1
    LOOP

    IF key$ <> "A" THEN
      IF UCASE$(promptForAnyKey$("END of buffer reached. Wrap around? (Y/N)")) <> "Y" THEN
        EXIT SUB
      ENDIF
      promptMsg "Searching for "+strToReplaceAbbr$+"...", 1
    ENDIF
    wrappedAround% = 1
    row% = 0
    col% = 0
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
    IF bufLinePtrs%(ii%, UNDO_BIDX%) THEN
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
  'Hack: boost key count to make sure pending undo records are aborted.
  keyCounter% = keyCounter% + 2
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
  
  ii%=0
  DO WHILE ii% < bufNumRows%(UNDO_BIDX%)
    freeLine bufLinePtrs%(ii%, UNDO_BIDX%)
    ii% = ii%+1
  LOOP
  bufNumRows%(UNDO_BIDX%) = 0
  undoIdx% = 0
  'Hack: boost key count to make sure pending undo records are aborted.
  keyCounter% = keyCounter% + 2
END SUB

SUB saveFile
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  LOCAL row% = 0
  LOCAL lin$
  LOCAL ii%=NUM_BACKUP_FILES%

  promptMsg "Saving "+ bufFilename$(bIdx%) + " ...", 1
   
  IF DIR$(bufFilename$(bIdx%)+".ba"+STR$(ii%), FILE) <> "" THEN
    KILL bufFilename$(bIdx%)+".ba"+STR$(ii%)
  ENDIF
  
  DO WHILE ii% > 0
    IF ii% = 1 THEN
      IF DIR$(bufFilename$(bIdx%), FILE) <> "" THEN
        RENAME bufFilename$(bIdx%) AS bufFilename$(bIdx%)+".ba1"
      ENDIF
    ELSE
      IF DIR$(bufFilename$(bIdx%)+".ba"+STR$(ii%-1), FILE) <> "" THEN
        RENAME bufFilename$(bIdx%)+".ba"+STR$(ii%-1) AS bufFilename$(bIdx%)+".ba"+STR$(ii%)
      ENDIF
    ENDIF   
    ii% = ii% - 1
  LOOP
  
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
  LOCAL fname$
  
  IF ENABLE_FILE_DIALOG_BOX% THEN
    fname$ = SaveFileName(15,"*") 
  ELSE
    fname$ = promptForText$("Save Buffer as: ")
  ENDIF
  
  'Don't accept empty files
  IF (fname$ = "") OR (RIGHT$(fname$, 1) = "/") THEN
    EXIT SUB
  ENDIF

  bufFilename$(bIdx%) = fname$
  
  LOCAL fileExt$ = UCASE$(RIGHT$(bufFilename$(bIdx%),4))
  IF (fileExt$=".INC") OR (fileExt$=".BAS") OR (fileExt$="") THEN
    bufSynHLEnabled%(bIdx%) = DEFAULT_ENABLE_SYN_HL%
  ELSE
    bufSynHLEnabled%(bIdx%) = 0
  ENDIF

  saveFile
  
  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
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
  CONST SHOW_KEYB% = 1
  CONST SHOW_USR_CFG% = 2
  CONST EXIT_HELP% = 0  
  LOCAL state% = SHOW_KEYB%
  
  DO
    SELECT CASE state%
      CASE SHOW_KEYB%
        showKeybindPopup
        IF promptForAnyKey$("") = CRSR_RIGHT_INKEY$ THEN
          state% = SHOW_USR_CFG%
        ELSE
          state% = EXIT_HELP%
        ENDIF

      CASE SHOW_USR_CFG%
        showUserCfgPopup
        IF promptForAnyKey$("") = CRSR_LEFT_INKEY$ THEN
          state% = SHOW_KEYB%
        ELSE
          state% = EXIT_HELP%          
        ENDIF     
    END SELECT
    
    removePopup
  LOOP UNTIL state% = EXIT_HELP%
END SUB

SUB resourceUtilKeyHandler
  showResUtilPopup
  LOCAL dummy$ = promptForAnyKey$("")
  removePopup
END SUB

SUB screenshotKeyHandler
  LOCAL screenshotFileName$ = promptForText$("Screenshot Filename: ")
  promptMsg "Saving...", 1
  SAVE IMAGE screenshotFileName$
  promptMsg "Screenshot saved.", 1
END SUB

SUB startMacroRecKeyHandler
  macroRecEnabled% = 1
  macroRecordNumEntries% = 0
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

SUB toggleSynHL
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  bufSynHLEnabled%(bIdx%) = NOT bufSynHLEnabled%(bIdx%)
  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
  
  LOCAL otherWidx% = NOT crsrActiveWidx%
  LOCAL otherBidx% = winBuf%(otherWidx%)
  IF (otherBidx% = bIdx%) AND winVisible%(otherWidx%) THEN
    winRedrawAction%(otherWidx%) = FULL_REDRAW%
  ENDIF

END SUB

SUB showConsoleKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)

  IF NOT bufIsConsole%(bIdx%) THEN
    promptMsg "Displaying console screen...", 1
    
    IF NOT closeBuffer%() THEN
      EXIT SUB
    ENDIF
    
    setupBuffer bIdx%, MM.VRES\ROW_HEIGHT%, "(CONSOLE)", 1
    
    winBufCrsrCol%(crsrActiveWidx%) = 0
    winBufCrsrRow%(crsrActiveWidx%) = 0
    winWinCrsrCol%(crsrActiveWidx%) = 0
    winWinCrsrRow%(crsrActiveWidx%) = 0
    
    crsrOff
    
    promptMsg "Done.", 1
  ENDIF
END SUB

SUB runProgKeyHandler
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)

  IF UCASE$(RIGHT$(bufFilename$(bIdx%), 4)) <> ".BAS" THEN
    LOCAL ok$ = promptForAnyKey$("Buffer filename does not have a .BAS extension. Press any key to continue.")
    EXIT SUB
  ENDIF
  
  requestExit bufFilename$(bIdx%)
END SUB

SUB killToEOLkeyHandler
  winSelectCol%(crsrActiveWidx%) = winBufCrsrCol%(crsrActiveWidx%)
  winSelectRow%(crsrActiveWidx%) = winBufCrsrRow%(crsrActiveWidx%)
  
  endKeyHandler 1
  
  registerForUndo UNDO_DELETE_SELECTION%
  IF NOT deleteSelection%() THEN
    undoRegisterForUndo
    EXIT SUB
  ENDIF
END SUB

SUB selectAllKeyHandler
  winSelectCol%(crsrActiveWidx%) = 0
  winSelectRow%(crsrActiveWidx%) = 0
  endKeyHandler 3
  winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
END SUB

'topNotCenter=0 means move to center, =1 means move to top.
SUB repositionKeyHandler(topNotCenter%)
  LOCAL targetWinRow%
  
  IF topNotCenter% THEN
    targetWinRow% = 0 'Move to top
  ELSE
    targetWinRow% = winNumRows%(crsrActiveWidx%)\2 'Move to center
  ENDIF
  
  LOCAL linesToScroll% = winWinCrsrRow%(crsrActiveWidx%)-targetWinRow%

  IF winBufTopRow%(crsrActiveWidx%) + linesToScroll% < 0 THEN
    linesToScroll% = -winBufTopRow%(crsrActiveWidx%)
  ENDIF
  scrollVdelta(crsrActiveWidx%, linesToScroll%)
  winWinCrsrRow%(crsrActiveWidx%) = winWinCrsrRow%(crsrActiveWidx%) - linesToScroll%
  winBufCrsrRow%(crsrActiveWidx%) = winBufTopRow%(crsrActiveWidx%) + winWinCrsrRow%(crsrActiveWidx%)
END SUB
'<-- End of Key Handler section

'--> Keypress with modifiers and selection logic.

'Returns true if selection should be cleared by this keyPress before handling the keypress.
FUNCTION clrSelectionBeforeKey%(key%)
  SELECT CASE key%
    CASE CRSR_UP_KEY%, CRSR_DOWN_KEY%, CRSR_LEFT_KEY%, CRSR_RIGHT_KEY%, PGUP_KEY%, PGDOWN_KEY%, HOME_KEY%, END_KEY%
      IF (SERIAL_INPUT_COMPAT_MODE% = 1) AND selectionActive% THEN  
        clrSelectionBeforeKey% = 0
      ELSE
        clrSelectionBeforeKey% = 1
      ENDIF
    CASE ELSE
      clrSelectionBeforeKey% = 0
  END SELECT
END FUNCTION

'Returns true if selection should be cleared by this keyPress after handling the keypress.
FUNCTION clrSelectionAfterKey%(key%)
  IF isShiftNavKey%(key%) THEN
    clrSelectionAfterKey% = 0
    EXIT FUNCTION
  ENDIF

  SELECT CASE key%
    CASE INDENT_KEY%, UNINDENT_KEY%, UNDO_KEY%, SELECT_ALL_KEY% ', COPY_KEY% was here
      clrSelectionAfterKey% = 0
    CASE ELSE
      clrSelectionAfterKey% = 1
  END SELECT
END FUNCTION

'Returns true if given keycode is shift + one of the navigation keys
FUNCTION isShiftNavKey%(key%)
  IF SERIAL_INPUT_COMPAT_MODE% = 0 THEN  
    SELECT CASE key%
      CASE SELECT_CRSR_U_KEY%, SELECT_HOME_KEY%, SELECT_END_KEY%, SELECT_PGUP_KEY%, SELECT_PGDOWN_KEY%, SELECT_CRSR_D_KEY%
        isShiftNavKey% = 1
      CASE SELECT_CRSR_L_KEY%, SELECT_CRSR_R_KEY%
        isShiftNavKey% = 1
      CASE ELSE
        isShiftNavKey% = 0
    END SELECT
  ELSE 'SERIAL input mode: 
    SELECT CASE key%
      CASE TOGGLE_SELECTION_KEY%
        key%=0
        selectionActive% = selectionActive% XOR 1
      CASE CRSR_UP_KEY%, HOME_KEY%, END_KEY%, PGUP_KEY%, PGDOWN_KEY%, CRSR_DOWN_KEY%, CRSR_LEFT_KEY%, CRSR_RIGHT_KEY%
      CASE 0
      CASE ELSE
        selectionActive% = 0
    END SELECT    
    isShiftNavKey% = selectionActive%
  ENDIF
END FUNCTION

'Key handling in show console mode.
SUB handleKeyConsoleMode pressedKey%
  STATIC nHomes% = 0, nEnds% = 0
  LOCAL wIdx% = crsrActiveWidx%
  LOCAL prevTopRow% = winBufTopRow%(wIdx%)
  LOCAL prevTopCol% = winBufTopCol%(wIdx%)
  
  IF pressedKey% = HOME_KEY% THEN
    nHomes% = nHomes% + 1
  ELSE
    nHomes% = 0
  ENDIF

  IF pressedKey% = END_KEY% THEN
    nEnds% = nEnds% + 1
  ELSE
    nEnds% = 0
  ENDIF
  
  SELECT CASE pressedKey%
    CASE CRSR_UP_KEY%
      winBufTopRow%(wIdx%) = MAX(winBufTopRow%(wIdx%) - 1, 0)
    CASE CRSR_DOWN_KEY%
      winBufTopRow%(wIdx%) = MIN(winBufTopRow%(wIdx%) + 1, (MM.VRES\ROW_HEIGHT%) - winNumRows%(wIdx%))
    CASE CRSR_LEFT_KEY%
      winBufTopCol%(wIdx%) = MAX(winBufTopCol%(wIdx%) - 1, 0)
    CASE CRSR_RIGHT_KEY%
      winBufTopCol%(wIdx%) = MIN(winBufTopCol%(wIdx%) + 1, (MM.HRES\COL_WIDTH%) - winNumCols%(wIdx%))
    CASE HOME_KEY%
      IF nHomes% = 1 THEN
        winBufTopCol%(wIdx%) = 0
      ELSE
        winBufTopRow%(wIdx%) = 0
      ENDIF
    CASE END_KEY%
      IF nEnds% = 1 THEN
        winBufTopCol%(wIdx%) = (MM.HRES\COL_WIDTH%) - winNumCols%(wIdx%)
      ELSE
        winBufCrsrRow%(wIdx%) = (MM.VRES\ROW_HEIGHT%) - winNumRows%(wIdx%)
      ENDIF
    CASE PGUP_KEY%
      winBufCrsrRow%(wIdx%) = MAX(winBufCrsrRow%(wIdx%) - winNumRows%(wIdx%), 0)
    CASE PGDOWN_KEY%
      winBufCrsrRow%(wIdx%) = MAX(winBufCrsrRow%(wIdx%) + winNumRows%(wIdx%), (MM.VRES\ROW_HEIGHT%) - winNumRows%(wIdx%))
    CASE EXIT_KEY%
      exitKeyHandler
    CASE TOGGLE_SCREEN_SPLIT_KEY%
      toggleScreenSplitKeyHandler
    CASE TOGGLE_ACTIVE_WINDOW_KEY%
      toggleActiveWindowKeyHandler
    CASE TOGGLE_BUFFER_KEY%
      toggleBufferKeyHandler
    CASE LOAD_INTO_CURRENT_BUF_KEY%
      loadIntoCurrentBufKeyHandler
    CASE CLOSE_BUFFER_KEY%, SHOW_CONSOLE_KEY%
      closeBufferKeyHandler
    CASE HELP_KEY%
      helpKeyHandler
    CASE SCREENSHOT_KEY%
      screenshotKeyHandler
  END SELECT

  IF (prevTopRow% <> winBufTopRow%(wIdx%)) OR (prevTopCol% <> winBufTopCol%(wIdx%)) THEN  
    winRedrawAction%(wIdx%) = FULL_REDRAW%
  ENDIF
END SUB

'Key press dispatcher with support for selections and modifiers
SUB handleKey pressedKey%
  STATIC nConsecHomePresses% = 0, nConsecEndPresses% = 0, nSelectConsecPresses% = 0
    
  LOCAL bIdx% = winBuf%(crsrActiveWidx%)
  
  'Console mode has its own key handler
  IF bufIsConsole%(bIdx%) THEN
    handleKeyConsoleMode pressedKey%
    EXIT SUB
  ENDIF
  
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

  IF clrSelectionBeforeKey%(pressedKey%) THEN
    'Clear selection
    IF selectMode%(crsrActiveWidx%) THEN
      winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    ENDIF

    nSelectConsecPresses% = 0
    winSelectCol%(crsrActiveWidx%) = -1
    winSelectRow%(crsrActiveWidx%) = -1
  ENDIF

  IF selectMode%(crsrActiveWidx%) THEN
    IF clrSelectionAfterKey%(pressedKey%) THEN
      'Dont't clear yet but signal full redraw so key handlers can take that
      'into account.
      winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
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
      findKeyHandler 1
    CASE FIND_REV_KEY%
      findKeyHandler -1      
    CASE FIND_NEXT_KEY%
      findNextKeyHandler
    CASE FIND_PREV_KEY%
      findPrevKeyHandler    
    CASE FIND_ACROSS_FILES_KEY%
      findAcrossFilesKeyHandler
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
    CASE TOGGLE_SYN_HL%
      toggleSynHL
    CASE SHOW_CONSOLE_KEY%
      showConsoleKeyHandler
    CASE RUN_PROG_KEY%
      runProgKeyHandler
    CASE KILL_TO_EOL_KEY%
      killToEOLkeyHandler
    CASE RESOURCE_UTIL_KEY%
      resourceUtilKeyHandler
    CASE SELECT_ALL_KEY%
      selectAllKeyHandler
    CASE MOVE_TO_CENTER_KEY%
      repositionKeyHandler 0 '0=center
    CASE MOVE_TO_TOP_KEY%
      repositionKeyHandler 1 '1=top
    CASE ELSE
      pressedKey% = pressedKey% AND 255
      IF isPrintable%(pressedKey%) THEN
        'This is for the non-ctrl keys, i.e. the edits.
        editKeyHandler CHR$(pressedKey%)
      ENDIF
  END SELECT

  IF clrSelectionAfterKey%(pressedKey%) THEN
    'Clear selection
    IF selectMode%(crsrActiveWidx%) THEN
      winRedrawAction%(crsrActiveWidx%) = FULL_REDRAW%
    ENDIF

    nSelectConsecPresses% = 0
    winSelectCol%(crsrActiveWidx%) = -1
    winSelectRow%(crsrActiveWidx%) = -1
  ENDIF

  crsrOn
END SUB

'This is essentially a replacement for INKEY, with support for modifiers.
SUB checkKeyAndModifier
  IF SERIAL_INPUT_COMPAT_MODE% = 0 THEN  
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
  ELSE 'SERIAL_INPUT_COMPAT_MODE
    LOCAL k$ = INKEY$
    IF k$ = chr$(13) THEN 'IF CR then consume LF as well
      k$ = INKEY$
    ENDIF
    IF k$<>"" THEN
      handleKey ASC(k$)
    ENDIF
  ENDIF
END SUB

