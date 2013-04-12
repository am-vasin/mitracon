*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Cnt_Move      §à ¡®âç¨ª ƒ «¨­  „¬¨âà¨¥­ª®                 ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                   Š®«¨ç¥áâ¢® ¯¥à¥¨¬¥­®¢ ­¨© ¯®§¨æ¨©.                   ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 19.04.2002 ÄÄÙ
PROCEDURE Cnt_Move
PRIVATE s_sav, d01, d02, mss, cnt_s

s_sav = SELECT()
d01 = DATE()
d02 = DATE()

DO G_Period WITH d01, d02
SELECT 0
USE (base_path+"STO_LOG")
COUNT TO cnt_s FOR DAT_CORR >= d01 .AND. DAT_CORR <= d02
USE
IF cnt_s = 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"‡  ¯¥à¨®¤ á "+DTOC(d01)+" ¯® "+DTOC(d02)+" ¯¥à¥¨¬¥­®¢ ­¨ï ¯®§¨æ¨©"
  mss(3) = CHR(0)+" ­¥ ¯à®¢®¤¨«¨áì."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"‡  ¯¥à¨®¤ á "+DTOC(d01)+" ¯® "+DTOC(d02)+" ¡ë«® ¯à®¢¥¤¥­® "+ALLTRIM(STR(cnt_s))
  mss(3) = CHR(0)+" ¯¥à¥¨¬¥­®¢ ­¨© ¯®§¨æ¨©."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF  

SELECT(s_sav)
RETURN