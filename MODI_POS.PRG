*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Modi_Pos     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ��玻オ罔�/甌Г��┘ ��Ж罔� ���キ���矣琺.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.09.1999 様�
PROCEDURE Modi_Pos
PARAMETERS cd, d_prf, d_name, d_dev, d_div, d_class, d_spec

PRIVATE prf, nm, dev, dev_n, div, div_n, clss, class_n, spec, s_sav, pck, nt1, nt2, s_sale
PRIVATE c_sw, d_sw, f_sw, r_ret, addr1, addr2, nt, nm1, prf1, prd1, is_serv
r_ret = 0

s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_9906 AGAIN
IF cd # 0
  IF .NOT. SEEK(cd)
    USE
    SELECT(s_sav)
    RETURN
  ENDIF
  prf  = PREFIX
  nm   = NAME
  dev  = PRO_CODE
  div  = DIV_CODE
  clss = CLASS
  spec = IIF(STOP_PUR = "S", 2, 1)
  s_sale = .NOT. EMPTY(STOP_SALE)
  pck  = QNT_ROUND
  nt1  = MLINE(NOTE,1)
  nt2  = MLINE(NOTE,2)
  addr1 = ADDRESS
  addr2 = MEMO_ST
  is_serv = .NOT. EMPTY(WK_SW2)
ELSE
  prf  = IIF(TYPE("d_prf") = "C", d_prf, "")
  prf  = PADL(ALLTRIM(prf), FSIZE("PREFIX"))
  nm   = IIF(TYPE("d_name") = "C", d_name, "")
  nm   = PADR(nm, FSIZE("NAME"))
  dev  = IIF(TYPE("d_dev") $ "NC", d_dev, 1)
  div  = IIF(TYPE("d_div") = "N", d_div, 0)
  clss = IIF(TYPE("d_class") = "C", d_class, " ")
  spec = IIF(TYPE("d_spec") = "C", d_spec, " ")
  spec = IIF(spec = "S", 2, 1)
  s_sale = .F.
  pck  = 1
  nt1  = ""
  nt2  = ""
  addr1 = SPACE(FSIZE("ADDRESS"))
  addr2 = SPACE(FSIZE("MEMO_ST"))
  is_serv = .F.
ENDIF
SELECT 0
IF TYPE("dev") = "N"
  USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS TMP_9906 AGAIN
  SEEK dev
  dev_n = PADR(B_NAME,8)
ELSE
  USE (base_path+"DEVELOP") ORDER TAG B_NAME ALIAS TMP_9906 AGAIN
  IF SEEK(PADL(dev, FSIZE("B_NAME", "TMP_9906")))
    dev_n = PADR(B_NAME,8)
    dev = CODE
  ELSE
    dev_n = SPACE(8)
    dev = 1
  ENDIF
ENDIF
USE (base_path+"DIVISION") ORDER TAG CODE ALIAS TMP_9906 AGAIN
SEEK div
div_n = PADR(B_NAME,FSIZE("B_NAME"))
USE (base_path+"CLASSES") ORDER TAG CODE ALIAS TMP_9906 AGAIN
SEEK clss
class_n = PADR(NAME,FSIZE("NAME"))
nt1 = PADR(nt1,64)
nt2 = PADR(nt2,64)
STORE .F. TO c_sw, d_sw, f_sw
USE
SELECT STO_9906
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽� ��爲�牀→� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖.... ....:....!....:....!....:....!....:....!....:.... ....:....崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�     [ ] ��Гカ ....:....!....:.    ���皚�痰� 祚���→� 999999     �
*楽� [ ] ���瘁 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�   敖 �むメ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�   �   �甅�↓�� ....:...    �������皀�讚覃 ....:....!....:.   �   �
*�   青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*�    敖 ������珥 ��爲�牀→� 陳陳陳陳陳陳�                          �
*�    � ( ) ����珥�覃  ( ) �皀腑痰▲��覃 �     [ ] ���爛� �牀���    �
*�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰                          �
*楽� 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�                     [ ] �甄磽� �� ぎ痰�→�                       �
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 22, 70, IIF(cd = 0, "��Г��┘", "��玻オ罔�")+" ��Ж罔� ���キ���矣琺"
DO Sun_Bord WITH  2,  2,  4, 67, " ��爲�牀→� "
DO Sun_Bord WITH  6,  2,  8, 67, "           "
DO Sun_Bord WITH  9,  5, 11, 64, " �むメ� "
DO Sun_Bord WITH 12,  6, 14, 41, " ������珥 ��爲�牀→� "
DO Sun_Bord WITH 15,  2, 18, 67, " 踸�ョ��┘ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  3 GET prf
@  3,  8 GET nm SIZE 1, 50
******@  2, 46 GET f_sw PICTURE "@*C ��爼�" VALID Choose(1)
@  3, 59 GET dev_n VALID Develop(dev)
@  5,  7 GET d_sw PICTURE "@*C ��Гカ" VALID Choose(2) WHEN TYPE("let_div") = "C"
@  5, 18 GET div_n WHEN .F.
@  5, 38 SAY "���皚�痰� 祚���→�" GET pck PICTURE "999999"
@  6,  5 GET c_sw PICTURE "@*C ���瘁" VALID Choose(3)
@  7,  3 GET class_n WHEN .F.
@ 10,  9 SAY "�甅�↓��" GET addr1 PICTURE Addr_Pic()
@ 10, 30 SAY "�������皀�讚覃" GET addr2
@ 13,  8 GET spec PICTURE "@*RH ����珥�覃 ;�皀腑痰▲��覃"
@ 13, 47 GET s_sale PICTURE "@*C ���爛� �牀���" WHEN .F.
@ 16,  3 GET nt1
@ 17,  3 GET nt2
@ 19, 23 GET is_serv PICTURE "@*C �甄磽� �� ぎ痰�→�"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  PRIVATE np, i0, i1
  IF EMPTY(nt1) .AND. EMPTY(nt2)
    nt = ""
  ELSE
    nt = ALLTRIM(nt1)+CHR(13)+CHR(10)+ALLTRIM(nt2)
  ENDIF
  i0 = 1
  DO WHILE i0 <= LEN(nm)
    IF SUBSTR(nm, i0, 1) $ "0123456789"
      EXIT
    ENDIF
    i0 = i0+1
  ENDDO
  IF i0 <= LEN(nm)
    i1 = LEN(nm)
    DO WHILE i1 >= 1
      IF SUBSTR(nm, i1, 1) $ "0123456789"
        EXIT
      ENDIF
      i1 = i1-1
    ENDDO
    np = SUBSTR(nm, i0, i1-i0+1)
  ELSE
    np = ""
  ENDIF
  np = CHRTRAN(nm, n_digit, "")
  *
  * �矗���硅��ガ ゛���
  prf1 = SeekName(prf)
  nm1  = SeekName(nm)
  prd1 = SeekName(dev_n)
  IF cd = 0
    SET ORDER TO TAG CODE
    GO BOTTOM
    cd = CODE+1
    APPEND BLANK
    REPLACE CODE      WITH cd,    ;
            PRO_CODE  WITH dev,   ;
            PRODUCER  WITH dev_n, ;
            CLASS     WITH clss,  ;
            ADDRESS   WITH addr1, ;
            MEMO_ST   WITH addr2, ;
            PREFIX    WITH prf,   ;
            PREFIX1   WITH prf1,  ;
            NAME1     WITH nm1,   ;
            PRODUCER1 WITH prd1,  ;
            NAME      WITH nm,    ;
            NUM_PART  WITH np,    ;
            DIV_CODE  WITH div,   ;
            QNT_ROUND WITH pck,   ;
            WK_SW2    WITH IIF(is_serv, "*", ""), ;
            STOP_SALE WITH IIF(s_sale, "*", ""), ;
            STOP_PUR  WITH IIF(spec = 1, " ", "S"), ;
            DATE_ON   WITH DATE(), ;
            WHO_ADD   WITH user, ;
            NOTE      WITH nt
    r_ret = RECNO()
  ELSE
    SET ORDER TO TAG CODE
    SEEK cd
    IF PRO_CODE # dev .OR. ;
       PREFIX   # prf .OR. ;
       NAME     # nm
      SELECT 0
      USE (base_path+"STO_LOG") ALIAS SL_9906 AGAIN
      APPEND BLANK
      REPLACE CODE     WITH cd,               ;
              OLD_PRF  WITH STO_9906.PREFIX,  ;
              OLD_NAM  WITH STO_9906.NAME,    ;
              OLD_PRO  WITH STO_9906.PRO_CODE,;
              NEW_PRF  WITH prf, ;
              NEW_NAM  WITH nm,  ;
              NEW_PRO  WITH dev, ;
              DAT_CORR WITH DATE(),;
              TIM_CORR WITH TIME(),;
              WHO_CORR WITH user,  ;
              WHERE_C  WITH arm
      USE
      SELECT STO_9906
    ENDIF

    REPLACE PRO_CODE  WITH dev,   ;
            PRODUCER  WITH dev_n, ;
            CLASS     WITH clss,  ;
            PREFIX    WITH prf,   ;
            NAME      WITH nm,    ;
            PREFIX1   WITH prf1,  ;
            NAME1     WITH nm1,   ;
            PRODUCER1 WITH prd1,  ;
            NUM_PART  WITH np,    ;
            DIV_CODE  WITH div,   ;
            QNT_ROUND WITH pck,   ;
            STOP_SALE WITH IIF(s_sale, "*", ""), ;
            STOP_PUR  WITH IIF(spec = 1, " ", "S"), ;
            WK_SW2    WITH IIF(is_serv, "*", ""), ;
            NOTE      WITH nt
    r_ret = RECNO()
    IF addr1 # ADDRESS
      REPLACE ADDR_1  WITH ADDR_0,  ;
              ADDR_0  WITH ADDRESS, ;
              ADDRESS WITH addr1
    ENDIF
    IF addr2 # MEMO_ST
      REPLACE MEMO_ST_1  WITH MEMO_ST_0,  ;
              MEMO_ST_0  WITH MEMO_ST,    ;
              MEMO_ST    WITH addr2
    ENDIF

  ENDIF
ENDIF
*--------------------------------------------------------------------------
USE
SELECT (s_sav)
POP KEY
RELEASE WINDOW (win_name)
RETURN r_ret

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襦�� ├ 甎�瓷�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.09.1999 陳�
PROCEDURE Choose
PARAMETERS m

DO CASE
CASE m = 1
  IF .NOT. f_sw
    RETURN .T.
  ENDIF
  f_sw = .F.
  DO Ch_Dev WITH dev, dev_n
  SHOW GET f_sw
  SHOW GET dev_n
CASE m = 2
  IF .NOT. d_sw
    RETURN .T.
  ENDIF
  d_sw = .F.
  DO Ch_Div WITH div, div_n
  SHOW GET d_sw
  SHOW GET div_n
CASE m = 3
  IF .NOT. c_sw
    RETURN .T.
  ENDIF
  c_sw = .F.
  DO Ch_Class WITH clss, class_n
  SHOW GET c_sw
  SHOW GET class_n
ENDCASE

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     蹍▲爲� ��玻オ皚�痰� ゛����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.09.1999 陳�
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

PRIVATE mss
prf = PADL(ALLTRIM(prf),4)

IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 2
ENDIF

SET ORDER TO TAG NAME
IF cd = 0
  IF SEEK(nm+prf+STR(dev,5))
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��������� ���� ��━キ����┘ �� 祗┴��讚�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN 2
  ENDIF
ELSE
  SET FILTER TO CODE # cd
  IF SEEK(nm+prf+STR(dev,5))
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��������� ���� ��━キ����┘ �� 祗┴��讚�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SET FILTER TO
    RETURN 2
  ENDIF
  SET FILTER TO
ENDIF
IF EMPTY(class_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"｡錻�皀�讚� 甄イ礇� 礫���碎 ���瘁!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 7
ENDIF
IF pck <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���皚�痰� 祚���→� ぎ�Ν� °碎 ｀�跏� �祀�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 6
ENDIF
IF .F. && div = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� ��Гカ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 5
ENDIF

RETURN .T.
