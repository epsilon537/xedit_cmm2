'-->User Configurable Settings - defaults:
'Copy this file to "settings.user.inc" to get a
'user config file that will not be 
'overwritten by future updates.
'If a "settings.user.inc file exists", it always
'takes precedence over "settings.default.inc".
'---------------------------------------------
'Solarized color scheme
CONST base03%  = RGB(&H00,&H2b,&H36)
CONST base02%  = RGB(&H07,&H36,&H42)
CONST base01%  = RGB(&H58,&H6e,&H75)
CONST base00%  = RGB(&H65,&H7b,&H83)
CONST base0%   = RGB(&H83,&H94,&H96)
CONST base1%   = RGB(&H93,&Ha1,&Ha1)
CONST base2%   = RGB(&Hee,&He8,&Hd5)
CONST base3%   = RGB(&Hfd,&Hf6,&He3)
CONST yellow%  = RGB(&Hb5,&H89,&H00)
CONST orange%  = RGB(&Hcb,&H4b,&H16)
CONST red%     = RGB(&Hdc,&H32,&H2f)
CONST magenta% = RGB(&Hd3,&H36,&H82)
CONST violet%  = RGB(&H6c,&H71,&Hc4)
CONST blue%    = RGB(&H26,&H8b,&Hd2)
CONST cyan%    = RGB(&H2a,&Ha1,&H98)
CONST green%   = RGB(&H85,&H99,&H00)

'Solarized dark color scheme
CONST FG_COLOR% = base0% 'RGB(WHITE)
CONST FG_COLOR2% = base1% 'RGB(CYAN)
CONST KEYWORD_COLOR% = yellow% 'RGB(YELLOW)
CONST STRING_COLOR% = blue% 'RGB(0, 255, 0)
CONST COMMENT_COLOR% = cyan% 'RGB(255,0,255)

CONST BG_COLOR% = base03% 'RGB(0,0,128)
CONST BG_COLOR2% = base01% 'RGB(64, 64, 255)

'If you prefer something higher contrast:
'CONST FG_COLOR% = RGB(230, 230, 230)
'CONST FG_COLOR2% = RGB(CYAN)
'CONST KEYWORD_COLOR% = RGB(220,220,100)
'CONST STRING_COLOR% = RGB(0, 255, 255)
'CONST COMMENT_COLOR% = RGB(255,0,255)

'CONST BG_COLOR% = RGB(0,0,128)
'CONST BG_COLOR2% = RGB(64, 64, 255)

CONST MAX_NUM_CMDLINE_ARGS% = 2

CONST KEYB_REPEAT_FIRST% = 300
CONST KEYB_REPEAT_REST% = 40

CONST DEFAULT_ENABLE_SYN_HL% = 1

'Set to 1 to make the search function case sensitive
CONST SEARCH_IS_CASE_SENSITIVE% = 0
CONST TAB_WIDTH% = 2
'Set to 1 to try and restore previous context (open files and cursor positions) when open XEdit.
CONST RESTORE_PREV_SESSION_CTXT% = 0
CONST CTXT_FILE_PATH$ = "\.xedit.ctxt"

'Disable those pesky confirmation prompts
CONST DISABLE_CONFIRMATION_PROMPTS% = 0

'Set to 1 to accept input from serial port. Note that this affects certain keybindings.
'In particular, selection mode is toggled using the Esc key. See help screen for details.
CONST SERIAL_INPUT_COMPAT_MODE% = 0

'Set to the number of backup copies you would like to maintain when saving a file.
CONST NUM_BACKUP_FILES% = 0

'Set to 0 to open and save files through prompt instead of a File Dialog box.
CONST ENABLE_FILE_DIALOG_BOX% = 1

'<--User Configurable Settings
'-----------------------------

