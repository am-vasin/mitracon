*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ��玻オ罔�/甌Г���� ��┘���.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Modi_Cus
PARAMETERS mode

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

PRIVATE ex, nm_w, sw_i, sw_a, cd, rc_sav, tmpType, tmpINN, tmpKPP
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
nm_w = IIF(mode = "A", SPACE(FSIZE("CUS_NAME")), CUS_NAME)
sw_i = IIF(mode = "A",.T.,.F.)
sw_a = .F.
m.tmpINN = ""
m.tmpKPP = ""
IF mode = "A"
  tmpType = 0
  DO Get_INN WITH 0, m.tmpINN, m.tmpKPP
ELSE
  DO Get_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP
  DO CASE
  CASE MARK1 = "1"
    tmpType = 2
  CASE MARK1 = "2"
    tmpType = 3
  OTHERWISE
    tmpType = 1
  ENDCASE
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��� ��┘��� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�        ��� ....:....!..    ��� ....:....!....:.        �
*�                                                        �
*�      [ ] ��珥�腦� ��┘���    [ ] �矗─竄� ��┘���      �
*�                                                        �
*�     ( ) �爬�.  ( ) 踳く爬�━�皀��  ( ) ��痰�.��罧     �
*�                                                        �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

DO Prp_Nav_2
DO D_Win_N WITH 13, 60, "��┘��"
DO Sun_Bord WITH 2, 3, 4, WCOLS()-4
@ 2, 5 SAY " ��� ��┘��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  5 GET nm_w
@  5, 10 SAY "���" GET m.tmpINN
@  5, 30 SAY "���" GET m.tmpKPP
@  7,  8 GET sw_i PICTURE "@*C ��珥�腦� ��┘���" VALID Tst_Inf()
@  7, 32 GET sw_a PICTURE "@*C �矗─竄� ��┘���"
@  9,  7 GET tmpType PICTURE "@*RH �爬�. ;踳く爬�━�皀�� ;��痰�. ��罧"
@ 11, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Name()

tmpType = TRANSFORM(tmpType-1, "@Z 9")
IF ex = 1
  IF mode = "A"
    IF (sys_char = "A" .OR. sale_mode = "Mitin")   && �甅�↓�� �����お�: ぎ��←錺� ����瘡

      cd = Doc_Num("CUS_CODE  ", "", arm, DATE())
      SELECT ACCOUNT
      APPEND BLANK
      REPLACE SUBSYS     WITH IIF(EMPTY(pl_wk), sys_char, pl_wk),  ;
              CUS_CODE   WITH cd,        ;
              CUS_NAME   WITH nm_w,      ;
              DATE_CORR  WITH DATE(),    ;
              MARK1      WITH tmpType,   ;
              WHO_CORR   WITH user,      ;
              WHERE_CORR WITH arm,       ;
              DATE_ON    WITH DATE(),    ;
              WHO        WITH user,      ;
              WHERE      WITH arm

    ELSE     && �� �甅�↓�� �����お�: �甎��讌礇� ����皰→�

      rc_sav = RECNO()
      SET ORDER TO TAG EMP_NAME
      GO TOP
      IF EOF() .OR. BOF()
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"�� �瘍ム���� ≡� ����皰→�!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        sw_i = .F.
        sw_a = .F.
      ELSE
        REPLACE SUBSYS     WITH sys_char,  ;
                CUS_NAME   WITH nm_w,      ;
                WHO_CORR   WITH user,      ;
                WHERE_CORR WITH arm,       ;
                DATE_CORR  WITH DATE(),    ;
                MARK1      WITH tmpType,   ;
                DATE_ON    WITH DATE(),    ;
                WHO        WITH user,      ;
                WHERE      WITH arm
        rc_sav = RECNO()
      ENDIF
      SET ORDER TO TAG CUS_NAME
      IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
        GO rc_sav
      ENDIF
    ENDIF

  ELSE

    REPLACE CUS_NAME   WITH nm_w, ;
            WHO_CORR   WITH user, ;
            MARK1      WITH tmpType,   ;
            WHERE_CORR WITH arm,  ;
            DATE_CORR  WITH DATE()

  ENDIF
  DO Put_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP
  IF sw_i
    DO Cli_Info WITH "", .T.
  ENDIF
  IF sw_a
    DO Cus_Bil WITH ACCOUNT.CUS_CODE, ACCOUNT.CUS_NAME
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍▲爲� ━キ� ��┘���.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Name

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"鉑皰� ━� ��┘���? ��� �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF .NOT. INLIST(tmpType, 1, 2, 3)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��� � �� 礫����� 皋� ��┘���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN Tst_INN(IIF(mode = "A", 0, CUS_CODE), m.tmpINN )

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�          ｡錻�皀�讚覃 □�� 痰���� �爬 ��▲ぅ�┬ ��〓�� ��┘���.        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 26.03.2003 夕
PROCEDURE Tst_Inf

IF mode = "A" 
  sw_i = .T.
ENDIF

SHOW GET sw_i

RETURN
