'*****************************************************************
'xFind integration:
'*****************************************************************
CONST XFIND_MAX_NUM_ROWS% = 2000

' xFind globals
DIM xFindBufRow%
DIM xFindBIdx%
DIM recursionLevel% = 0
DIM startDir$ = CWD$
DIM matchCounter%

SUB xFind(bIdx%, strToSearch$, fspec$)
  LOCAL ok%
  
  xFindBufRow% = 0
  xFindBIdx% = bIdx%
  matchCounter% = 0
  
  ok% = wrBufLine%(bIdx%, xFindBufRow%, "Searching for: "+strToSearch$)
  INC xFindBufRow%
  ok% = wrBufLine%(bIdx%, xFindBufRow%, "In: "+fspec$)
  INC xFindBufRow%

  'We can't just flag a request for redraw here because we're not returning
  'to the mainloop (yet).
  drawWinContents crsrActiveWidx%
  drawWinHeader crsrActiveWidx%

  'File spec or directory given?
  LOCAL fileToScan$ = DIR$(fspec$, FILE)

  IF fileToScan$<>"" THEN
    'If a subdirectory is specified, cd into there first
    LOCAL bd$=baseDir$(fspec$)
    IF bd$<>"" THEN
      CHDIR bd$
    ENDIF
    
    DO WHILE fileToScan$ <> ""
      scanFile strToSearch$, fileToScan$
      fileToScan$ = DIR$()
    LOOP
  ELSEIF (fspec$=".") OR (DIR$(fspec$, DIR) <> "") THEN
    scanDir(strToSearch$, fspec$)
  ENDIF

  ok% = wrBufLine%(xFindBIdx%, xFindBufRow%, "Number of matches: "+STR$(matchCounter%))
  INC xFindBufRow%
  ok% = wrBufLine%(xFindBIdx%, xFindBufRow%, "Done.")
  INC xFindBufRow%

  CHDIR startDir$
END SUB

'This function starts an iteration over all sub directories in the current directory.
FUNCTION listDirs$()
  'It can be so easy...
  listDirs$ = DIR$("*", DIR)
END FUNCTION

'This function starts an iteration over all files in the current directory.
FUNCTION listFiles$()
  listFiles$ = DIR$("*.*", FILE)
END FUNCTION

'Extract the base directory portion of a filespec. E.g df/*.INC -> df
FUNCTION baseDir$(fspec$)
  LOCAL dividerPos%=0, prevDividerPos%
  
  baseDir$=""
  
  DO
    prevDividerPos% = dividerPos%
    dividerPos% = INSTR(dividerPos%+1, fspec$, "/")
    IF dividerPos%=0 THEN
      dividerPos% = INSTR(dividerPos%+1, fspec$, "\")
    ENDIF
  LOOP UNTIL dividerPos%=0
  
  IF prevDividerPos%<>0 THEN
    baseDir$ = (LEFT$(fspec$, prevDividerPos%))
  ENDIF
END FUNCTION

SUB scanFile(strToSearch$, filename$)
  'PRINT SPACE$(recursionLevel%*2) "Processing file " filename$
  
  OPEN filename$ FOR INPUT AS #1

  LOCAL lin$
  LOCAL lineNbr%=1
  LOCAL k$
  LOCAL ok%
  
  'Contents
  DO WHILE NOT EOF(#1)
    ON ERROR SKIP 1
    LINE INPUT #1, lin$
    IF MM.ERRNO = 0 THEN
      IF INSTR(UCASE$(lin$), UCASE$(strToSearch$)) THEN
        k$ = CWD$ + "/" + filename$ + " " + STR$(lineNbr%) + ": " + lin$
        ok% = wrBufLine%(xFindBIdx%, xFindBufRow%, k$)
        INC xFindBufRow%
        INC matchCounter%
        
        'We can't just flag a request for redraw here because we're not returning
        'to the mainloop (yet).
        drawWinContents crsrActiveWidx%
        drawWinHeader crsrActiveWidx%
      ENDIF
    ENDIF
    ON ERROR CLEAR
    INC lineNbr%
  LOOP

  CLOSE #1
END SUB

'This subroutine processes the contents of given directory
SUB scanDir(strToSearch$, dirToProcess$)
  recursionLevel% = recursionLevel% + 1

  LOCAL dirToProcess_l$ = dirToProcess$

  CHDIR dirToProcess_l$

  'Process the files
  LOCAL fileToProcess$ = listFiles$()

  DO WHILE fileToProcess$ <> ""
    scanFile strToSearch$, fileToProcess$
    fileToProcess$ = DIR$()
  LOOP

  'Process the subdirs  
  LOCAL subDir$ = listDirs$()

  'DIR$/nextDir$ can't handle recursion in this while loop so we have to build a subDir list  
  LOCAL numSubDirs% = 0

  'First calculate how many subdirs there are in this directory
  DO WHILE subDir$ <> ""
    numSubDirs% = numSubDirs% + 1
    subDir$ = DIR$()
  LOOP

  IF numSubDirs% >= 1 THEN
    'Note: The size of this array is too big by 1 entry
    LOCAL subDirList$(numSubDirs%)

    subDir$ = listDirs$()
    LOCAL listIdx% = 0

    DO WHILE subDir$ <> ""
      subDirList$(listIdx%) = subDir$
      subDir$ = DIR$()
      listIdx% = listIdx% + 1
    LOOP  

    'Now we recurse. For some reason this doesn't work with a while loop, 
    'but with a for loop it works just fine.
    FOR listIdx%=0 TO numSubDirs%-1
      scanDir strToSearch$, subDirList$(listIdx%)
    NEXT listIdx%
  ENDIF

  CHDIR ".."
  recursionLevel% = recursionLevel% - 1
END SUB

