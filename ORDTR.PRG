*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� OrdTr        ��о�｀砒┴ ��むォ ��瓱�           26.02.97 13:29:25 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         蹍������ �÷�｀皖� ������ � 瓣�｀き�� DBF - 筮爼�皀.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE OrdTr

PRIVATE s_file, arr_f, arr_f1, arr_w, f, what_do, p_, n, i, s_sav

*
*    �皖琺��ガ � �瘁�イ礇� �痳�き覃 ����
*
s_file = Int_File("", "DBF", "���Θ皀 �痳�き覃 DBF-����")

IF EMPTY(s_file)
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
USE (s_file) ALIAS S_FILE

DIMENSION p_(7)
p_(1) = 1
p_(2) = 1
p_(3) = 1
p_(4) = 1
p_(5) = 1
p_(6) = 1
p_(7) = 1

n = AFIELDS(arr_w)
DIMENSION arr_f(n+1), arr_f1(n+1)
arr_f(1) = SPACE(10)
arr_f1(1) = SPACE(10)
FOR i = 1 TO n
  arr_f(i+1)  = IIF(arr_w(i,2) = "C", "", "\")+PADR(arr_w(i,1), 10)
  arr_f1(i+1) = IIF(arr_w(i,2) $ "NC", "", "\")+PADR(arr_w(i,1), 10)
ENDFOR

SELECT 0
SET EXCLUSIVE ON
DELETE FILE (tmpo_path+"T_ORD_.DBF")
CREATE DBF (tmpo_path+"T_ORD_")  ;
   (  DOC_NUM   N( 1),  ;
      DOC_DATE  D,      ;
      PREFIX    C( 4),  ;
      NAME      C(78),  ;
      PRODUCER  C( 8),  ;
      MODEL     C(10),  ;
      FUNC      C(20),  ;
      QNT       N( 6),  ;
      PRICE     N(12,4)  )
SET EXCLUSIVE OFF


PRIVATE what_do
what_do = "Prep"

DO WHILE .T.

  DO CASE

  CASE what_do = "Prep"    && ��ぃ�皰→� ∇ガキ���� �����

    what_do = IIF(Prep(), "List", "Abort")

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    IF V_List()
      EXIT
    ELSE
      what_do = "Abort"
    ENDIF

  OTHERWISE
  
    SELECT S_FILE
    USE
    SELECT T_ORD_
    USE
    DELETE FILE (tmpo_path+"T_ORD_.DBF")
    SELECT (s_sav)
    RETURN

  ENDCASE

ENDDO

SELECT S_FILE
USE
i = RAT(".", s_file)
f = LEFT(s_file, i)+"OLD"

*IF File_O(f)
*  DELETE FILE (f)
*ENDIF

*RENAME (s_file) to (f)

SELECT T_ORD_
USE

SELECT (s_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍������ 甌��痰�←キ�� ���ォ.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.97 14:04:08 夕
PROCEDURE Prep
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

PRIVATE ex, p_w, n_w, sw_
DIMENSION sw_(7)
sw_(1) = .F.
sw_(2) = .F.
sw_(3) = .F.
sw_(4) = .F.
sw_(5) = .F.
sw_(6) = .F.
sw_(7) = .F.
ex  = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                            �
*� [ ] 踳筥��     ....:....!     [ ] ��━キ����┘ ....:....! �
*�                                                            �
*� [ ] ��爼�       ....:....!     [ ] ���-〓       ....:....! �
*�                                                            �
*� [ ] ��ぅ�� ���. ....:....!     [ ] �祗��. ��Л. ....:....! �
*�                                                            �
*�                     [ ] �キ� ....:....!                    �
*�                                                            �
*�              < OK Ctrl_W > < �皖���碎瘴 Esc >              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰


*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 64, "����痰��赳� ����"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 GET sw_(1) PICTURE "@*C 踳筥��" VALID G_Field("踳筥��",1,"C",3,19) DISABLED
@ 3, 34 GET sw_(2) PICTURE "@*C ��━キ����┘" VALID G_Field("��━キ����┘",2,"C",3,51)
@ 5,  3 GET sw_(3) PICTURE "@*C ��爼�" VALID G_Field("��爼�",3,"C",5,19)
@ 5, 34 GET sw_(4) PICTURE "@*C ���-〓" VALID G_Field("���-〓",4,"N",5,51)
@ 7,  3 GET sw_(5) PICTURE "@*C ��ぅ�� ���." VALID G_Field("��ぅ�� ���.",5,"C",7,19)
@ 7, 34 GET sw_(6) PICTURE "@*C �祗��. ��Л." VALID G_Field("�祗��. ��Л.",6,"C",7,51)
@ 9, 23 GET sw_(7) PICTURE "@*C �キ�" VALID G_Field("�キ�",7,"N",9,32) DISABLE
@ 11, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            蹍▲爲� ���ォ.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.97 16:45:47 夕
PROCEDURE Tst_Blank

PRIVATE mss, f_p, f_n, f_q, f_a, f_f, f_b, f_pr, a_w, i

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF p_(2) = 1
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �￥��皀�讚� ��ぎ 礫���碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF p_(4) = 1
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����腑痰〓 �￥��皀�讚� ��ぎ 礫���碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT T_ORD_
ZAP
SELECT S_FILE
SCAN
  SCATTER TO a_w
  i = p_(1)
  f_p = IIF(i = 1, "", a_w(i-1))
  i = p_(2)
  f_n = IIF(i = 1, "", a_w(i-1))
  i = p_(3)
  f_b = IIF(i = 1, "", a_w(i-1))
  i = p_(4)
  f_q = IIF(i = 1, 0, a_w(i-1))
  i = p_(5)
  f_a = IIF(i = 1, "", a_w(i-1))
  i = p_(6)
  f_f = IIF(i = 1, "", a_w(i-1))
  i = p_(7)
  f_pr = IIF(i = 1, 0, a_w(i-1))
  IF TYPE("f_q") # "N"
    f_q = VAL(ALLTRIM(f_q))
  ENDIF
  IF TYPE("f_q") # "N"
    f_pr = VAL(ALLTRIM(f_pr))
  ENDIF
  f_p = PADL(ALLTRIM(f_p),4)
  SELECT T_ORD_
  APPEND BLANK
  REPLACE PREFIX   WITH f_p, ;
          NAME     WITH f_n, ;
          PRODUCER WITH f_b, ;
          MODEL    WITH f_a, ;
          FUNC     WITH f_f, ;
          QNT      WITH f_q, ;
          PRICE    WITH f_pr
  SELECT S_FILE
ENDSCAN

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                蹍甃�矗 � ��玻オ罔� 甌Г������ 甎�瓷�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.97 16:55:38 夕
PROCEDURE V_List

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
PRIVATE ret_v
ret_v = .T.

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�〓� 甎�瓷�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

SELECT T_ORD_
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("PREFIX")+35+FSIZE("PRODUCER")+FSIZE("QNT")+19

DO D_Wins WITH ln, wd, "蹍甃�矗 � ��玻オ罔� ��錢��", 0, 0
@ 1, 2 SAY ;
"陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����-〓陳陳陳陳��キ�"
*PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 999999 9999999
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", NAME:35:H="", PRODUCER:H="", QNT:H="", PRICE:H="":P="9999999999.9999"   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = IIF(READKEY() % 256 = 12, "exit", "Choose")

  CASE what_do = "Choose"    && �襦��

    ret_v = .T.
    EXIT

  OTHERWISE

    ret_v = .F.
    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

RETURN ret_v

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Field      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                              �襦�� ����.                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/10/1999 陳�
PROCEDURE G_Field
PARAMETERS tit, i0, tp, l_0, c_0
IF .NOT. sw_(i0)
  RETURN .T.
ENDIF

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

PRIVATE p_w
p_w = p_(i0)
ln = MIN(ALEN(arr_f)+3, 20)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, 14, tit

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  p_w = MIN(p_w, ALEN(arr_f))
  p_w = MAX(p_w, 1)
  IF tp = "C"
    @ 1, 1 MENU arr_f, ALEN(arr_f), ln-3
  ELSE
    @ 1, 1 MENU arr_f1, ALEN(arr_f1), ln-3
  ENDIF
  READ MENU TO p_w
  IF READKEY() = 12
    RELEASE WINDOW (win_name)
    EXIT
  ENDIF
  IF READKEY() = 15
    p_(i0) = p_w
    RELEASE WINDOW (win_name)
    @ l_0, c_0 SAY arr_f1(p_(i0))
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------
POP KEY
sw_(i0) = .F.
SHOW GET sw_(i0)
*KEYBOARD "{Tab}"
RETURN .T.
