*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Dlv_Stat     맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                     뫌飡�輾�� 쭬琠え 췅 ㄾ飡젪ゃ.                      �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 28.03.2000 컴�
PROCEDURE Dlv_Stat
PARAMETERS d_num, d_date

PRIVATE d_year, s_sav

IF TYPE("d_date") = "D"
  d_year = LEFT(DTOS(d_date),4)
ENDIF
IF TYPE("d_date") = "N"
  d_year = STR(d_date,4)
ENDIF
IF TYPE("d_date") = "C"
  d_year = d_date
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0328 AGAIN
SELECT 0
USE (base_path+"DELIVERY") ORDER TAG DOC_NUM ALIAS DLV_0328 AGAIN

IF .NOT. SEEK(d_year+d_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"꽡ゃД�� �� 췅ħ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT WHO_0328
  USE
  SELECT (s_sav)
  RETURN
ENDIF

*
*  룯誓Д��瑜 貰飡�輾⑨ ㄻ� 췅˘짛與�
*

PRIVATE stat_type     && 뮜� 췅˘짛與�: 0 - �α�젺쩆設�硫;
                                        1 - ∥젺�;
                                        2 - BROWSE - �젩エ��;
                                        3 - BROWSE - 召ⓤ��.
PRIVATE what_do       && 닾� 誓┬쵟.
PRIVATE menu_name     && 닾� 졹Þ魚����． Д��.
PRIVATE last_mouse    && 귖�э ��笹ⅳ�ⅲ� 췅쬊殊� ㎖�黍��� き��え.
PRIVATE win_name      && 닾� �き� ( �ぎ� ㄻ� BROWSE ).

*
*   뇿��キ畑� ㎛좂��⑨Ж ��誓Д��瑜 貰飡�輾⑨...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    뫌ㄵ逝졻�レ췅� �졹筍 �昔｀젹щ:
*

*旼컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴쩡컴컴컴컴커
*� 뮙耶. 將젽      �  뫌循蝨�Ø                   �    꽑��    �  귖�э   �
*쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
*� 궋�� 쭬琠え     � ....:....!....:....!....:... � 꼧.뙆.긿긿 � 뿓:뙆:몣 �
*쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
*� 뒶薛.쭬琠え     � ....:....!....:....!....:... � 꼧.뙆.긿긿 � 뿓:뙆:몣 �
*쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
*� 룯�졻� 쭬琠え   � ....:....!....:....!....:... � 꼧.뙆.긿긿 � 뿓:뙆:몣 �
*쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
*� 귣㎜� ゃ閃���   � ....:....!....:....!....:... � 꼧.뙆.긿긿 � 뿓:뙆:몣 �
*쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
*� 롡��젪첓 ゃ閃��졂 ....:....!....:....!....:... � 꼧.뙆.긿긿 � 뿓:뙆:몣 �
*쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
*� 뇿む淫�� 쭬琠え � ....:....!....:....!....:... � 꼧.뙆.긿긿 � 뿓:뙆:몣 �
*읕컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴좔컴컴컴컴켸
*                                    < OK >                    

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && 뜝 ㏇篆Ł ��쬊惜硫 笹晨젵!
DO Prp_Nav_2
DO D_Win_N WITH 17, 76, "뫌飡�輾�� 쭬琠え N "+ALLTRIM(DOC_NUM)+" "+DTOC(DOC_DATE)

*------------------------------------------------------------------------
*      궋�� ��ゥ� ∥젺첓
*
@ 1, 0 CLEAR TO WROWS()-1, WCOLS()-1
@ 0, 0 SAY ""
TEXT
 旼컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴쩡컴컴컴컴커
 � 뮙耶. 將젽      �  뫌循蝨�Ø                   �    꽑��    �  귖�э   �
 쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
 � 궋�� 쭬琠え     �                              �            �          �
 쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
 � 뒶薛.쭬琠え     �                              �            �          �
 쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
 � 룯�졻� 쭬琠え   �                              �            �          �
 쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
 � 귣㎜� ゃ閃���   �                              �            �          �
 쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
 � 롡��젪첓 ゃ閃��졂                              �            �          �
 쳐컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴탠컴컴컴컴캑
 � 뇿む淫�� 쭬琠え �                              �            �          �
 읕컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴좔컴컴컴컴켸
ENDTEXT
PRIVATE nm_w
IF .NOT. SEEK(SALESMAN, "WHO_0328")
  nm_w = "?"
ELSE
  nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
ENDIF
@  4, 21 SAY nm_w
@  4, 52 SAY DTOC(ENTER_DAT)
@  4, 65 SAY ENTER_TIM

IF .NOT. EMPTY(MOD_DATE)
  IF .NOT. SEEK(WHO_MOD, "WHO_0328")
    nm_w = "?"
  ELSE
    nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  @  6, 21 SAY nm_w
  @  6, 52 SAY DTOC(MOD_DATE)
  @  6, 65 SAY MOD_TIME
ENDIF

IF .NOT. EMPTY(READ_DATE)
  IF .NOT. SEEK(WHO_READ, "WHO_0328")
    nm_w = "?"
  ELSE
    nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  @  8, 21 SAY nm_w
  @  8, 52 SAY DTOC(READ_DATE)
  @  8, 65 SAY READ_TIME
ENDIF

IF .NOT. EMPTY(CALL_DATE)
  IF .NOT. SEEK(WHO_CALL, "WHO_0328")
    nm_w = "?"
  ELSE
    nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  @ 10, 21 SAY nm_w
  @ 10, 52 SAY DTOC(CALL_DATE)
  @ 10, 65 SAY CALL_TIM
ENDIF

IF .NOT. EMPTY(GAVE_DATE)
  IF .NOT. SEEK(WHO_GAVE, "WHO_0328")
    nm_w = "?"
  ELSE
    nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  @ 12, 21 SAY nm_w
  @ 12, 52 SAY DTOC(GAVE_DATE)
  @ 12, 65 SAY GAVE_TIME
ENDIF

IF .NOT. EMPTY(TERM_DATE)
  IF .NOT. SEEK(WHO_TERM, "WHO_0328")
    nm_w = "?"
  ELSE
    nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  @ 12, 21 SAY nm_w
  @ 12, 52 SAY DTOC(TERM_DATE)
  @ 12, 65 SAY TERM_TIME
ENDIF

@ WROWS()-1, FLOOR(WCOLS()/2-3) GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE
SELECT WHO_0328
USE
SELECT (s_sav)
RETURN
