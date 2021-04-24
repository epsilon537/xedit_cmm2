FUNCTION swapDateFieldsInStr$(dateTimeStr$)
  LOCAL y$ = FIELD$(dateTimeStr$, 1, "- ")
  LOCAL m$ = FIELD$(dateTimeStr$, 2, "- ")
  LOCAL d$ = FIELD$(dateTimeStr$, 3, "- ")
  LOCAL t$ = MID$(dateTimeStr$, INSTR(dateTimeStr$," "))
  
  swapDateFieldsInStr$ = d$+"-"+m$+"-"+y$+t$
END FUNCTION

SUB checkForUserCopyIfNew(usr$, def$, target$)
  usr$ = MM.INFO(PATH)+usr$
  def$ = MM.INFO(PATH)+def$
  target$ = MM.INFO(PATH)+target$
  LOCAL src$ = CHOICE(DIR$(usr$, FILE) <> "", usr$, def$) 'User or default?

  LOCAL srcDateTime$ = swapDateFieldsInStr$(MM.INFO$(MODIFIED src$))
  LOCAL targetDateTime$ = swapDateFieldsInStr$(MM.INFO$(MODIFIED target$))
  
  IF EPOCH(srcDateTime$) > EPOCH(targetDateTime$) THEN 'Copy if src is newer than target.
    COPY src$ TO target$
  ENDIF
END SUB

checkForUserCopyIfNew "settings.user.inc", "settings.default.inc", "settings_dontedit.INC"
checkForUserCopyIfNew "keybindings.user.inc", "keybindings.default.inc", "keybindings_dontedit.INC"

EXECUTE "RUN "+CHR$(34)+MM.INFO(PATH)+"main.bas"+CHR$(34)+", "+MM.CMDLINE$
END


