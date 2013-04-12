*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  CopyCDX       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                  ‚®ááâ ­®¢«¥­¨¥ ¨­¤¥ªá®¢ ¯® íâ «®­ã.                   º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 06/23/2000 ÍÍ¼
PROCEDURE CopyCDX
PRIVATE s_sav, file_1, file_2, ex, i_expr, f_expr, t_name

s_sav = SELECT()
SELECT 0

file_1 = ""
file_2 = ""
ex     = 1

DO Int_P

IF ex # 1
  RETURN
ENDIF

SET EXCLUSIVE OFF
DO Set_Lwr
SET TALK OFF

USE (file_2) ALIAS PATT
SELECT 0
USE (file_1) ALIAS MAIN EXCLU
DELETE TAG ALL
SELECT PATT
ex = 1
t_name = TAG(ex)
DO WHILE .NOT. EMPTY(t_name)
  i_expr = SYS(14, ex)
  f_expr = SYS(2021, ex)
  IF .NOT. EMPTY(f_expr)
    f_expr = "FOR "+f_expr
  ENDIF
  SELECT MAIN
  INDEX ON &i_expr TAG (t_name) &f_expr ADDITIVE
  SELECT PATT
  ex = ex+1
  t_name = TAG(ex)
ENDDO
USE
SELECT MAIN
USE
SELECT (s_sav)

RETURN

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Int_P         §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                     ˆ­â¥à ªâ¨¢­ ï ç áâì ¯à®£à ¬¬ë.                     ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 06/23/2000 ÄÄÙ
PROCEDURE Int_P
PRIVATE ln, wd, l0, c0, ch1, ch2

ch1 = 1
ch2 = 1
ln  = 7
wd  = 68
l0 = FLOOR((WROWS("")-ln)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW CopyCDX FROM l0, c0 TO l0+ln-1, c0+wd-1 NONE SHADOW COLOR SCHEME 10
ACTIVATE WINDOW CopyCDX

@ 1,2 SAY "Š®àà¥ªâ¨àã¥¬ ï â ¡«¨æ "
@ 1,25 EDIT file_1 SIZE 1,31
@ 1,57 GET ch1 PICTURE "@*HN \ ‚ë¡®à " VALID G_File(1)
@ 3,2 SAY "â «®­­ ï â ¡«¨æ "
@ 3,25 EDIT file_2 SIZE 1,31
@ 3,57 GET ch2 PICTURE "@*HN \ ‚ë¡®à " VALID G_File(2)
@ 5,15 GET ex PICTURE "@*HT \!    OK Ctrl-W    ;\? Žâª § âìáï Esc "

READ CYCLE VALID Tst_Files()

RELEASE WINDOW CopyCDX

RETURN

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Tst_Files     §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                     à®¢¥àª  ª®àà¥ªâ­®áâ¨ ä ©«®¢.                      ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 06/23/2000 ÄÄÙ
PROCEDURE Tst_Files

PRIVATE tmpFile

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(file_1)
  WAIT WINDOW "‚ë ­¥ ãª § «¨ ª®àà¥ªâ¨àã¥¬ãî â ¡«¨æã!"
  RETURN 1
ENDIF

IF EMPTY(file_2)
  WAIT WINDOW "‚ë ­¥ ãª § «¨ íâ «®­­ãî â ¡«¨æã!"
  RETURN 3
ENDIF

tmpFile = ALLTRIM(file_1)+IIF("." $ file_1, "", ".dbf")
IF .NOT. File_O(tmpFile)
  WAIT WINDOW "” ©« "+tmpFile+" ­¥ áãé¥áâ¢ã¥â!"
  RETURN 1
ENDIF

tmpFile = ALLTRIM(file_2)+IIF("." $ file_2, "", ".dbf")
IF .NOT. File_O(tmpFile)
  WAIT WINDOW "” ©« "+tmpFile+" ­¥ áãé¥áâ¢ã¥â!"
  RETURN 3
ENDIF

RETURN .T.

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  G_File        §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                              ‚ë¡®à ä ©« .                              ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 06/23/2000 ÄÄÙ
PROCEDURE G_File
PARAMETERS mode

PRIVATE tmpFile
tmpFile = IIF(mode = 1, file_1, file_2)

tmpFile = GetFile("DBF")

IF .NOT. EMPTY(tmpFile)
  IF mode = 1
    file_1 = tmpFile
    SHOW GET file_1
  ELSE
    file_2 = tmpFile
    SHOW GET file_2
  ENDIF
ENDIF

RETURN .T.

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Set_Lwr       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                ’ ¡«¨æ  ¯¥à¥ª®¤¨à®¢ª¨ ¢ ­¨¦­¨© à¥£¨áâà.                 ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 06/23/2000 ÄÄÙ
PROCEDURE Set_Lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32," ¡¢£¤¥¦§¨©ª«¬­®¯àáâãäåæçèéêëìíîï")
RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º             à®£à ¬¬  ¯à®¢¥àª¨ ­ «¨ç¨ï ä ©« .                         º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE File_O
PARAMETERS f_nam

PRIVATE arr_w
RETURN adir(arr_w, f_nam) > 0
