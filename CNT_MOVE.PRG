*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Cnt_Move     맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                   뒶エ曄飡¡ ��誓º���쥯�Ł ��㎤與�.                   �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 19.04.2002 컴�
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
  mss(2) = CHR(0)+"뇿 ��黍�� � "+DTOC(d01)+" �� "+DTOC(d02)+" ��誓º���쥯�⑨ ��㎤與�"
  mss(3) = CHR(0)+" �� �昔¡ㄸエ刷."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"뇿 ��黍�� � "+DTOC(d01)+" �� "+DTOC(d02)+" 〓ギ �昔´ㄵ�� "+ALLTRIM(STR(cnt_s))
  mss(3) = CHR(0)+" ��誓º���쥯�Ł ��㎤與�."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF  

SELECT(s_sav)
RETURN