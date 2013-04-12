*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Changes1      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                        ®¨áª § ¬¥­ ¤«ï ¯®§¨æ¨¨.                        º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 11.09.2001 ÍÍ¼
PROCEDURE Changes1
PARAMETERS position, resArr, flags, aAnalog, aStock, aSubject
*
*  position - ª®¤ ¯®§¨æ¨¨;
*  resArr   - ¬ áá¨¢, ¢ ª®â®àë© ¯®¬¥é ¥âáï à¥§ã«ìâ â:
*             ª®¤ ¯®§¨æ¨¨, â¨¯ § ¬¥­ë, ãà®¢¥­ì § ¬¥­ë, ª®à¯ãá(ª®¤);
*  flags    - ä« ¦ª¨ ¤«ï â¨¯®¢ § ¬¥­;
*  aAnalog  -  «¨ á ¤«ï ä ©«   ­ «®£®¢;
*  aStock   -  «¨ á ¤«ï ä ©«  ­®¬¥­ª« âãàë;
*  aSubject -  «¨ á ¤«ï ä ©«  SUBJECT
*

PRIVATE s_sav, r_part, l_arr, j, sCorpus, flagA, flagR, flagP
l_arr = 0
STORE .T. TO flagA, flagR, flagP
IF TYPE("flags") = "C"
  flagA = .NOT. EMPTY(SUBSTR(flags,1,1))
  flagR = .NOT. EMPTY(SUBSTR(flags,2,1))
  flagP = .NOT. EMPTY(SUBSTR(flags,3,1))
ENDIF

IF .NOT. flagA .AND. .NOT. flagR .AND. .NOT. flagP
  RETURN .F.
ENDIF

s_sav = SELECT()
SELECT 0

*
*   ˆé¥¬ ¢ á¯¨áª¥  ­ «®£®¢
*
IF flagA
  IF TYPE("aAnalog") = "C"
    SELECT (aAnalog)
    SET ORDER TO TAG CODE
  ELSE
    USE (base_path+"ANALOG") ORDER TAG CODE ALIAS AN_9924 AGAIN
  ENDIF
  IF SEEK(position)
    SCAN REST WHILE position = CODE_1
      IF Dubl(CODE_2)
        LOOP
      ENDIF
      l_arr = l_arr+1
      DIMENSION resArr(l_arr, 4)
      resArr(l_arr, 1) = CODE_2
      resArr(l_arr, 2) = "A"
      resArr(l_arr, 3) = ""
      resArr(l_arr, 4) = 0
    ENDSCAN
  ENDIF
  IF TYPE("aAnalog") = "C"
    SELECT 0
  ELSE
    USE
  ENDIF
ENDIF

*
*    ëâ ¥¬áï á®¡à âì ¯®§¨æ¨¨ à §­ëå ¯à®¨§¢®¤¨â¥«¥©
*
IF flagP
  IF TYPE("aStock") = "C"
    SELECT (aStock)
    SET ORDER TO TAG CODE
  ELSE
    USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS ST_9924
  ENDIF
  IF SEEK(position)  && ®§¨æ¨ï ¥áâì ¢ ä ©«¥
    r_part = NAME+PREFIX
    SET ORDER TO TAG NAME
    SEEK r_part
    SCAN REST WHILE r_part = NAME+PREFIX FOR CODE # position  && —â®¡ë ­¥ ãª § âì ­  á¥¡ï
      IF Dubl(CODE)
        LOOP
      ENDIF
      l_arr = l_arr+1
      DIMENSION resArr(l_arr, 4)
      resArr(l_arr, 1) = CODE
      resArr(l_arr, 2) = "P"
      resArr(l_arr, 3) = ""
      resArr(l_arr, 4) = 0
    ENDSCAN
    IF TYPE("aSubject") = "C"
      SELECT 0
    ELSE
      USE
    ENDIF
  ENDIF
ENDIF

*
*    ëâ ¥¬áï á®¡à âì  ­ «®£¨ ¯® ª®à­ï¬
*
IF TYPE("aSubject") = "C"
  SELECT (aSubject)
  SET ORDER TO TAG STO_CODE
ELSE
  IF File_O(path_comm+"SUBJECT.DBF")
    USE (path_comm+"SUBJECT") ORDER TAG STO_CODE AGAIN ALIAS RT_9924
  ENDIF
ENDIF
IF TYPE("aSubject") = "C" .OR. USED("RT_9924")
  SET FILTER TO STO_CODE # 0
  IF flagR
    IF SEEK(position)  && ®§¨æ¨ï ¥áâì ¢ ä ©«¥
      r_part = ROOT
      IF .NOT. EMPTY(r_part)    && € ¡ë« «¨ ª®à¥­ì!?
        SET ORDER TO TAG ROOT
        SEEK r_part
        SCAN REST WHILE r_part = ROOT FOR STO_CODE # position   && —â®¡ë ­¥ ãª § âì ­  á¥¡ï
          IF Dubl(STO_CODE)
            LOOP
          ENDIF
          l_arr = l_arr+1
          DIMENSION resArr(l_arr, 4)
          resArr(l_arr, 1) = STO_CODE
          resArr(l_arr, 2) = "R"
          resArr(l_arr, 3) = ""
          resArr(l_arr, 4) = 0
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF
  IF l_arr > 0
    SET ORDER TO TAG STO_CODE
    SEEK position
    sCorpus = COR_CODE
    FOR j = 1 TO l_arr
      IF SEEK(resArr(j, 1))
        r_part = COR_CODE
        IF sCorpus # 0 .AND. COR_CODE # 0 .AND. sCorpus # COR_CODE
          r_part = -r_part
        ENDIF
        resArr(j, 4) = r_part
      ENDIF
    ENDFOR
  ENDIF
  IF TYPE("aSubject") = "C"
    SELECT 0
  ELSE
    USE
  ENDIF
ENDIF

SELECT (s_sav)

RETURN l_arr > 0

*****************************************************************
PROCEDURE Dubl
PARAMETER tmpCode
PRIVATE i
IF l_arr = 0
  RETURN .F.
ENDIF
FOR i = 1 TO l_arr
  IF tmpCode = resArr(i, 1)
    RETURN .T.
  ENDIF
ENDFOR

RETURN .F.
