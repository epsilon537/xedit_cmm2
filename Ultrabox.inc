'<--Everything below this point is Get/SaveFile logic from vegipete, with modification

' GetFileName Demonstration Program
' Shows usage for the GetFileName dialog function.
'
' by vegipete, October 2020
'   version 1.0   Original release
'   version 1.1   all navigation by arrow keys only, can return directory names too
'   version 1.2   remembers selection when moving back up directories, can specify
'                 file criteria, forcing selection type
'

CONST DIRCOUNT% = 50   ' max number of sub-directories
CONST FILCOUNT% = 255  ' max number of files
CONST NAMELENGTH% = 64
dim dir_dirs$(DIRCOUNT%) length NAMELENGTH%  ' store list of directories
dim dir_fils$(FILCOUNT%) length NAMELENGTH%  ' store list of files
dim dir_hist$(DIRCOUNT%) length 8 ' store directory number visited along path
dim d_lines%
dim d_x%, d_y%, d_back%, d_frame%

'*****************************************************************
' Function GetFileName(vsize%,spec$)
'
'   version 1.0    Original release vegipete, Oct 2020
'   version 1.1    all navigation by arrow keys only, can return directory names too
'
' This funtion displays a centered dialog box on the screen, allows
' the user to choose a file and returns the full path of the chosen
' file. The underlying screen is restored when the dialog closes.
' UP and DOWN arrows to select, ENTER to choose selection
' ESC to cancel, LEFT arrow to go up directory
'
' Input:  vsize%: number of directory items to list vertically
'         spec$: currently unused, hopefully later it will allow file filtering
'
' Output: string containing full path of file chosen, or "" if nothing
'         Note: the directory part of the path will be capitalized. This is just
'         how the CWD$ function works. Fortunately, MMBasic is case insensitive.
'
' The following global variables should be declared before use:
' CONST DIRCOUNT% = 50   ' max number of sub-directories
' CONST FILCOUNT% = 255  ' max number of files
' CONST NAMELENGTH% = 64
' dim dir_dirs$(DIRCOUNT%) length NAMELENGTH%  ' store list of directories
' dim dir_fils$(FILCOUNT%) length NAMELENGTH%  ' store list of files
' dim dir_hist$(DIRCOUNT%) length 8 ' store directory number visited along path
' dim dir_notroot  ' used internally to indicate root directory or not
'
' Routines Used:  (included below)
'   sub ReadDir   ' reads current directory into the above arrays
'   sub ListDir(first, nlines, hilite)  ' shows a portion of the current directory
'
function GetFileName(vsize%,spec$) as string
  '--> Epsilon added:
  local ii%
  local fname$
  '<-- Epsilon added
  
  ' dialog box dimensions
  local d_shadow% = BG_COLOR2% 'Epsilon modified colors
  d_frame% =  FG_COLOR2% 'Epsilon modified colors
  d_back%  =  BG_COLOR% 'Epsilon modified colors
  d_lines% = vsize%
  local d_height% = 50 + (d_lines% - 1) * MM.INFO(FONTHEIGHT)
  local d_width% = 300
  d_x% = (MM.HRES - d_width%)/2
  d_y% = (MM.VRES - d_height%)/2

  local d_startdir$ = cwd$      ' save starting directory
  local d_k%
  local d_top_item%, d_sel_item%, d_top_last%, d_changed%, d_chosen%
  
  for d_k% = 1 to DIRCOUNT% ' set all elements to 1 - 1st item selected
    dir_hist$(d_k%) = "1,1"
  next d_k%
  dir_hist$(0) = "1"      ' initially at top directory level
  if d_startdir$ <> "A:/" then  ' determine starting directory depth
    d_startdir$ = d_startdir$ + "/"
    for d_k% = 1 to len(d_startdir$)
      if mid$(d_startdir$,d_k%,1) = "/" then
        dir_hist$(0) = str$(val(dir_hist$(0)) + 1) ' another level deeper
      endif
    next d_k%
  endif

  ' save underlying screen image in buffer #64
  blit read 64, d_x%, d_y%, d_width%, d_height%
  ' draw dialog box
  rbox d_x% + 7, d_y% +  7, d_width% -  8, d_height% -  8, 10, d_shadow%, d_shadow%
  rbox d_x%    , d_y%     , d_width% -  8, d_height% -  8, 10,  d_frame%, d_frame%
  rbox d_x% + 5, d_y% + 22, d_width% - 18, d_height% - 34,  5,   d_back%, d_back%  ' text area
  if ucase$(spec$) = "<DIR>" then
    text d_x%+10,d_y%+6,"Select Directory...", "LT", 1, 1, 0, -1
  else
    text d_x%+10,d_y%+6,"Select File...", "LT", 1, 1, 0, -1
  endif
  dir_dirs$(0)=chr$(146)+chr$(147)+chr$(149)+" "+chr$(148)+"/Ent/Esc/N(ew)"   'temp
  text d_x%+d_width%-15,d_y%+6,dir_dirs$(0), "RT", 1, 1, 0, -1

  '--------------------
  ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
  ListDir(d_top_item%, d_lines%, d_sel_item%)  ' populate the dialog box

  do
    d_k% = asc(inkey$)
    d_changed% = 0
    select case d_k%
      case  27  ' ESC
        GetFileName$ = ""  ' Cancel so return blank
        exit do
      case 128  ' UP arrow
        if d_sel_item% = 1 then  ' is the top item selected?
          if d_top_item% > 1 then  ' at top of list?
            d_top_item% = d_top_item% - 1  ' no so shift list up one
            d_changed% = 1
          endif
        else
          d_sel_item% = d_sel_item% - 1  ' shift selection up one
          d_changed% = 1
        endif
      case 129  ' DOWN arrow
        if d_sel_item% = d_lines% then  ' is the bottom item selected?
          if d_top_item% < d_top_last% then  ' at bottom of list?
            d_top_item% = d_top_item% + 1  ' no so shift list down one
            d_changed% = 1
          endif
        else if d_sel_item% < val(dir_dirs$(0)) + val(dir_fils$(0)) then
          ' don't shift down past last item
          d_sel_item% = d_sel_item% + 1  ' shift selection down one
          d_changed% = 1
        endif
      case 130  ' LEFT Arrow - directory up if not root
        if cwd$ <> "A:/" then ' in a sub-directory?
          chdir ".."     'directory up chosen
          ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
          dir_hist$(0) = str$(val(dir_hist$(0)) - 1)
          d_top_item% = val(field$(dir_hist$(val(dir_hist$(0))),1,","))
          d_sel_item% = val(field$(dir_hist$(val(dir_hist$(0))),2,","))
          d_changed% = 1
        endif
      case 131  ' RIGHT Arrow - directory down if directory selected
        d_chosen% = d_top_item% + d_sel_item% - 1
        if d_chosen% <= val(dir_dirs$(0)) then ' item number in directory range?

          dir_hist$(val(dir_hist$(0))) = str$(d_top_item%) + "," + str$(d_sel_item%)
          'dir_hist$(dir_hist$(0)) = d_chosen    ' save selection number if we come back up
          dir_hist$(0) = str$(val(dir_hist$(0)) + 1)

          if right$(cwd$,1) = "/" then
            chdir cwd$ + dir_dirs$(d_chosen%)  ' tunnel down a directory from root
          else
            chdir cwd$ + "/" + dir_dirs$(d_chosen%)  ' tunnel down a directory
          endif
          ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
          d_changed%ù = 1
        endif
      case  13  ' ENTER - something has been selected
        d_chosen% = d_top_item% + d_sel_item% - 1
        if d_chosen% <= val(dir_dirs$(0)) then ' item number in directory range?
          if ucase$(spec$) = "<DIR>" then   ' was directory selection chosen?
            if right$(cwd$,1) = "/" then
              GetFileName$ = cwd$ + dir_dirs$(d_chosen%) + "/"  ' directory at root level
            else
              GetFileName$ = cwd$ + "/" + dir_dirs$(d_chosen%) + "/"   ' directory deeper
            endif     ' Note: cwd$ returns all uppercase
            exit do
          endif
        else    ' Yahoo! A filename has been chosen
          if ucase$(spec$) <> "<DIR>" then   ' was other than directory selection chosen?
            d_chosen% = d_chosen% - val(dir_dirs$(0))
            if dir_fils$(d_chosen%) <> "" then  ' in case directory has no (specified) file
              if right$(cwd$,1) = "/" then
                GetFileName$ = cwd$ + dir_fils$(d_chosen%)  ' filename at root level
              else
                GetFileName$ = cwd$ + "/" + dir_fils$(d_chosen%)  ' filename deeper
              endif     ' Note: cwd$ returns all uppercase
              exit do
            endif
          endif
        endif
      '--> Epsilon added:
      case ASC("n"), ASC("N")
        if d_top_item% + d_sel_item% - 1 <= val(dir_dirs$(0)) then
          d_sel_item% = val(dir_dirs$(0)) - d_top_item% + 2
        endif
        d_chosen% = d_top_item% + d_sel_item% - 1 - val(dir_dirs$(0))
        
        'Insert an empty line at selection point
        ii% = val(dir_fils$(0))
        do while ii% >= d_chosen%
          dir_fils$(ii%+1) = dir_fils$(ii%)
          ii% = ii%-1
        loop
        dir_fils$(d_chosen%) = ""
        dir_fils$(0) = str$(val(dir_fils$(0))+1)
        
        'Relist dir with the empty line.
        ListDir(d_top_item%, d_lines%, d_sel_item%)

        'Position cursor at the empty line and get the input
        print @(d_x%+14, d_y%+24+(d_sel_item%-1)*MM.INFO(FONTHEIGHT)) SPACE$(33);
        print @(d_x%+14, d_y%+24+(d_sel_item%-1)*MM.INFO(FONTHEIGHT)) "";
        input "", fname$

        if right$(cwd$,1) = "/" then
          fname$ = cwd$ + fname$  ' filename at root level
        else
          fname$ = cwd$ + "/" + fname$  ' filename deeper
        endif     ' Note: cwd$ returns all uppercase
        
        if dir$(fname$) <> "" then
          print @(d_x%+14, d_y%+24+(d_sel_item%-1)*MM.INFO(FONTHEIGHT), 2) "File Exists!                    ";
          do while inkey$<> "": loop: do while inkey$= "": loop
          
          'Delete the empty line at selection point again
          ii% = d_chosen%
          do while ii% < val(dir_fils$(0))
            dir_fils$(ii%) = dir_fils$(ii%+1)
            ii% = ii%+1
          loop
          dir_fils$(ii%) = ""
          dir_fils$(0) = str$(val(dir_fils$(0))-1)
          
          'Relist dir again
          ListDir(d_top_item%, d_lines%, d_sel_item%)
        else            
          GetFileName$ = fname$
          exit do
        endif
        '<-- Epsilon added.
    end select
    if d_changed% then   ' something changed so redisplay directory list
      ListDir(d_top_item%, d_lines%, d_sel_item%)
    endif
  loop
  '--------------------

  ' restore original screen image
  box d_x%, d_y%, d_width%, d_height%, 1, 0, 0 ' must clear to black first
  blit write 64, d_x%, d_y%   ' now restore all non-black pixels
  blit close 64

  ' restore starting directory
  chdir d_startdir$

end function

'--> Epsilon added:
'*****************************************************************
' Function SaveFileName(vsize%,spec$)
'
function SaveFileName(vsize%,spec$) as string
  local ii%
  local fname$
  
  ' dialog box dimensions
  local d_shadow% = BG_COLOR2% 'Epsilon modified colors
  d_frame% =  FG_COLOR2% 'Epsilon modified colors
  d_back%  =  BG_COLOR% 'Epsilon modified colors

  d_lines% = vsize%
  local d_height% = 50 + (d_lines% - 1) * MM.INFO(FONTHEIGHT)
  local d_width% = 400
  d_x% = (MM.HRES - d_width%)/2
  d_y% = (MM.VRES - d_height%)/2

  local d_startdir$ = cwd$      ' save starting directory
  local d_k%
  local d_top_item%, d_sel_item%, d_top_last%, d_changed%, d_chosen%

  for d_k% = 1 to DIRCOUNT% ' set all elements to 1 - 1st item selected
    dir_hist$(d_k%) = "1,1"
  next d_k%
  dir_hist$(0) = "1"      ' initially at top directory level
  if d_startdir$ <> "A:/" then  ' determine starting directory depth
    d_startdir$ = d_startdir$ + "/"
    for d_k% = 1 to len(d_startdir$)
      if mid$(d_startdir$,d_k%,1) = "/" then
        dir_hist$(0) = str$(val(dir_hist$(0)) + 1) ' another level deeper
      endif
    next d_k%
  endif

  ' save underlying screen image in buffer #64
  blit read 64, d_x%, d_y%, d_width%, d_height%
  ' draw dialog box
  rbox d_x% + 7, d_y% +  7, d_width% -  8, d_height%ù -  8, 10, d_shadow%, d_shadow%
  rbox d_x%    , d_y%     , d_width% -  8, d_height% -  8, 10,  d_frame%, d_frame%
  rbox d_x% + 5, d_y% + 22, d_width% - 18, d_height% - 34,  5,   d_back%, d_back%  ' text area
  text d_x%+10,d_y%+6,"Save File...", "LT", 1, 1, 0, -1
 
  dir_dirs$(0)=chr$(146)+chr$(147)+chr$(149)+" "+chr$(148)+"/Enter (New Filename)/Esc"   'temp
  text d_x%ù+d_width%-15,d_y%+6,dir_dirs$(0), "RT", 1, 1, 0, -1

  '--------------------
  ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
  ListDir(d_top_item%, d_lines%, d_sel_item%)  ' populate the dialog box

  do
    d_k% = asc(inkey$)
    d_changed% = 0
    select case d_k%
      case  27  ' ESC
        SaveFileName$ = ""  ' Cancel so return blank
        exit do
      case 128  ' UP arrow
        if d_sel_item% = 1 then  ' is the top item selected?
          if d_top_item% > 1 then  ' at top of list?
            d_top_item% = d_top_item% - 1  ' no so shift list up one
            d_changed%ù = 1
          endif
        else
          d_sel_item% = d_sel_item% - 1  ' shift selection up one
          d_changed% = 1
        endif
      case 129  ' DOWN arrow
        if d_sel_item% = d_lines% then  ' is the bottom item selected?
          if d_top_item% < d_top_last% then  ' at bottom of list?
            d_top_item% = d_top_item% + 1  ' no so shift list down one
            d_changed% = 1
          endif
        else if d_sel_item% < val(dir_dirs$(0)) + val(dir_fils$(0)) then
          ' don't shift down past last item
          d_sel_item% = d_sel_item% + 1  ' shift selection down one
          d_changed% = 1
        endif
      case 130  ' LEFT Arrow - directory up if not root
        if cwd$ <> "A:/" then ' in a sub-directory?
          chdir ".."     'directory up chosen
          ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
          dir_hist$(0) = str$(val(dir_hist$(0)) - 1)
          d_top_item% = val(field$(dir_hist$(val(dir_hist$(0))),1,","))
          d_sel_item% = val(field$(dir_hist$(val(dir_hist$(0))),2,","))
          d_changed% = 1
        endif
      case 131  ' RIGHT Arrow - directory down if directory selected
        d_chosen% = d_top_item% + d_sel_item% - 1
        if d_chosen% <= val(dir_dirs$(0)) then ' item number in directory range?

          dir_hist$(val(dir_hist$(0))) = str$(d_top_item%) + "," + str$(d_sel_item%)
          'dir_hist$(dir_hist$(0)) = d_chosen    ' save selection number if we come back up
          dir_hist$(0) = str$(val(dir_hist$(0)) + 1)

          if right$(cwd$,1) = "/" then
            chdir cwd$ + dir_dirs$(d_chosen%)  ' tunnel down a directory from root
          else
            chdir cwd$ + "/" + dir_dirs$(d_chosen%)  ' tunnel down a directory
          endif
          ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
          d_changed% = 1
        endif
      case 13
        if d_top_item% + d_sel_item% - 1 <= val(dir_dirs$(0)) then
          d_sel_item% = val(dir_dirs$(0)) - d_top_item% + 2
        endif
        d_chosen% = d_top_item% + d_sel_item% - 1 - val(dir_dirs$(0))
        
        'Insert an empty line at selection point
        ii% = val(dir_fils$(0))
        do while ii% >= d_chosen%
          dir_fils$(ii%+1) = dir_fils$(ii%)
          ii% = ii%-1
        loop
        dir_fils$(d_chosen%) = ""
        dir_fils$(0) = str$(val(dir_fils$(0))+1)
        
        'Relist dir with the empty line.
        ListDir(d_top_item%, d_lines%, d_sel_item%)

        'Position cursor at the empty line and get the input
        print @(d_x%+14, d_y%+24+(d_sel_item%-1)*MM.INFO(FONTHEIGHT)) SPACE$(33);
        print @(d_x%+14, d_y%+24+(d_sel_item%-1)*MM.INFO(FONTHEIGHT)) "";
        input "", fname$

        if right$(cwd$,1) = "/" then
          fname$ = cwd$ + fname$  ' filename at root level
        else
          fname$ = cwd$ + "/" + fname$  ' filename deeper
        endif     ' Note: cwd$ returns all uppercase
        
        if dir$(fname$) <> "" then
          print @(d_x%+14, d_y%+24+(d_sel_item%-1)*MM.INFO(FONTHEIGHT), 2) "File Exists!                    ";
          do while inkey$<> "": loop: do while inkey$= "": loop
          
          'Delete the empty line at selection point again
          ii% = d_chosen%
          do while ii% < val(dir_fils$(0))
            dir_fils$(ii%) = dir_fils$(ii%+1)
            ii% = ii%+1
          loop
          dir_fils$(ii%) = ""
          dir_fils$(0) = str$(val(dir_fils$(0))-1)
          
          'Relist dir again
          ListDir(d_top_item%, d_lines%, d_sel_item%)
        else            
          SaveFileName$ = fname$
          exit do
        endif
    end select
    if d_changed% then   ' something changed so redisplay directory list
      ListDir(d_top_item%, d_lines%, d_sel_item%)
    endif
  loop
  '--------------------

  ' restore original screen image
  box d_x%, d_y%, d_width%, d_height%, 1, 0, 0 ' must clear to black first
  blit write 64, d_x%, d_y%   ' now restore all non-black pixels
  blit close 64

  ' restore starting directory
  chdir d_startdir$

end function
'<-- Epsilon added

'*****************************************************************
' Read directories and specified files in the current directory
sub ReadDir(spec$,d_top_item%,d_sel_item%,d_top_last%)
  local item_cnt%, i%

  for i% = 1 to DIRCOUNT%
    dir_dirs$(i%) = ""   ' clear the array
  next i%
  for i% = 1 to FILCOUNT%
    dir_fils$(i%) = ""   ' clear the array
  next i%

  ' read directories first
  dir_dirs$(0) = ""  ' 0 items to begin
  item_cnt% = 1
  dir_dirs$(item_cnt%) = left$(Dir$("*", DIR),NAMELENGTH%) ' WARNING - possible truncation
  Do While dir_dirs$(item_cnt%) <> "" and item_cnt% < DIRCOUNT% - 1
    If dir_dirs$(item_cnt%) <> "." Then item_cnt% = item_cnt% + 1 ' ignore "."
    dir_dirs$(item_cnt%) = Dir$()
  Loop
  if dir_dirs$(item_cnt%) = "" then item_cnt% = item_cnt% - 1

  ' Sort directories
  Sort dir_dirs$()    ' note:  "" < "A"
  ' shift non-blank entries to front of array
  for i% = 1 to item_cnt%
    dir_dirs$(i%) = dir_dirs$(DIRCOUNT%-item_cnt%+i%)
  next i%
  dir_dirs$(0) = str$(item_cnt%)   ' store number of items

  ' now read files
  dir_fils$(0) = ""  ' 0 items to begin
  item_cnt% = 1
  if ucase$(spec$) = "<DIR>" then
    dir_fils$(item_cnt%) = left$(Dir$("*", FILE),NAMELENGTH%) ' WARNING - possible truncation
  else
    dir_fils$(item_cnt%) = left$(Dir$(spec$, FILE),NAMELENGTH%) ' WARNING - possible truncation
  endif
  Do While dir_fils$(item_cnt%) <> "" and item_cnt% < FILCOUNT% - 1
    If dir_fils$(item_cnt%) <> "." Then item_cnt%ù = item_cnt% + 1 ' ignore "."
    dir_fils$(item_cnt%) = Dir$()
  Loop
  if dir_fils$(item_cnt%) = "" then item_cnt% = item_cnt% - 1

  ' Sort files and shift non-blank entries to front of array
  Sort dir_fils$()
  for i% = 1 to item_cnt%
    dir_fils$(i%) = dir_fils$(FILCOUNT%-item_cnt%+i%)
  next i%
  dir_fils$(0) = str$(item_cnt%)   ' store number of items

  d_top_item% = 1
  d_sel_item% = 1
  d_top_last% = val(dir_dirs$(0)) + val(dir_fils$(0)) - d_lines% + 1

end sub

'*****************************************************************
' Display (part of) directory
' Show 'nlines' number of items, starting with item 'first',
' hilite given item
' The magic number "33" should be altered to correspond to the magic
' number "300" used above for d_width.
sub ListDir(first%, nlines%, hilite%)
  local i%
  local item%
  local d_txt$
    
  for i% = 0 to nlines% - 1
    item% = first%ù + i%
    if item% > val(dir_dirs$(0)) then
      d_txt$ = dir_fils$(item% - val(dir_dirs$(0)))
    else
      d_txt$ = "<DIR> " + dir_dirs$(item%)
    endif
    if len(d_txt$) > 33 then d_txt$ = left$(d_txt$,32) + chr$(148)
    d_txt$ = left$(d_txt$ + space$(33),33)

    if i% = hilite% - 1 then
      text d_x%+14, d_y%+24+i%*MM.INFO(FONTHEIGHT), d_txt$,"LT",1,1,d_back%,d_frame%
    else
      text d_x%+14, d_y%+24+i%*MM.INFO(FONTHEIGHT), d_txt$,"LT",1,1,&hFFFFFF,d_back%
    endif
  next i%

end sub
'*****************************************************************

