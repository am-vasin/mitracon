*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� OrdTrans     ��о�｀砒┴ ��むォ ��瓱�           26.02.97 13:29:25 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         蹍������ �÷�｀皖� ������ � 瓣�｀き�� DBF - 筮爼�皀.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE OrdTrans

PRIVATE s_file, arr_f, arr_f1, arr_w, f, what_do, p_, n, i

*
*    �皖琺��ガ � �瘁�イ礇� �痳�き覃 ����
*
s_file = Int_File("", "DBF", "���Θ皀 �痳�き覃 DBF-����")

IF EMPTY(s_file)
  RETURN
ENDIF

USE (s_file) ALIAS S_FILE

DIMENSION p_(5)
p_(1) = 1
p_(2) = 1
p_(3) = 1
p_(4) = 1
p_(5) = 1

n = AFIELD(arr_w)
DIMENSION arr_f(n+1), arr_f1(n+1)
arr_f(1) = SPACE(10)
arr_f1(1) = SPACE(10)
FOR i = 1 TO n
  arr_f(i+1)  = IIF(arr_w(i,2) = "C", "", "\")+PADR(arr_w(i,1), 10)
  arr_f1(i+1) = IIF(arr_w(i,2) $ "NC", "", "\")+PADR(arr_w(i,1), 10)
ENDFOR

SELECT 0
SET EXCLUSIVE ON
DELETE FILE (tmpo_path+"T_ORD.DBF")
CREATE DBF (tmpo_path+"T_ORD")  ;
   (  DOC_NUM   N( 1),  ;
      DOC_DATE  D,      ;
      PREFIX    C( 4),  ;
      NAME      C(35),  ;
      MODEL     C(10),  ;
      FUNC      C(20),  ;
      QNT       N( 6)  )
SET EXCLUSIVE OFF


PRIVATE what_do
what_do = "Prep"

DO WHILE .T.

  DO CASE

  CASE what_do = "Prep"    && ��ぃ�皰→� ∇ガキ���� �����

    what_do = IIF(Prep(), "List", "Exit")

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    DO V_List
    what_do = "Save"

  CASE what_do = "Save"

    IF .NOT. Term_F()
      what_do = "Prep"
    ELSE
      what_do = "Exit"
    ENDIF

  OTHERWISE
  
    EXIT

  ENDCASE

ENDDO

CLOSE DATABASES
DELETE FILE (tmpo_path+"T_ORD.DBF")

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

PRIVATE ex, p_w, n_w
n_w = 1
ex  = 1

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽陳陳陳陳陳陳堕陳陳陳陳陳賃陳陳陳陳陳陳堕陳陳陳陳陳賃陳陳陳陳陳陳審
*崖  踳筥��   ���━キ����┘���ぅ�� ���. ��祗��. ��Л.� ����腑痰〓 崖
*鈎陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳陳干
*崖            �            �            �            �            崖
*崖            �            �            �            �            崖
*崖            �            �            �            �            崖
*垣陳陳陳陳陳陳祖陳陳陳陳陳珍陳陳陳陳陳陳祖陳陳陳陳陳珍陳陳陳陳陳陳抒
*�                                                                  �
*�                 < OK Ctrl_W > < �皖���碎瘴 Esc >                 �
*�                                                                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰


*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 70, "����痰��赳� ����"
p_w = p_(1)

@ 2, 2 SAY "敖陳陳陳陳陳賃陳陳陳陳陳陳堕陳陳陳陳陳賃陳陳陳陳陳陳堕陳陳陳陳陳朕"
@ 3, 2 SAY "�  踳筥��   ���━キ����┘���ぅ�� ���. ��祗��. ��Л.� ����腑痰〓 �"
@ 4, 2 SAY "団陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳陳調"
@ 5, 2 SAY "�            �            �            �            �            �"
@ 6, 2 SAY "�            �            �            �            �            �"
@ 7, 2 SAY "�            �            �            �            �            �"
@ 8, 2 SAY "青陳陳陳陳陳珍陳陳陳陳陳陳祖陳陳陳陳陳珍陳陳陳陳陳陳祖陳陳陳陳陳潰"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  5,  3 GET p_(1) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 16 GET p_(2) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 29 GET p_(3) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 42 GET p_(4) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 55 GET p_(5) PICTURE "@^" FROM arr_f1 SIZE 1, 12
@ 10, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

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

PRIVATE mss, f_p, f_n, f_q, f_a, f_f, a_w, i

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

SELECT T_ORD
ZAP

SELECT S_FILE
SCAN
  SCATTER TO a_w
  i = p_(1)
  f_p = IIF(i = 1, "", a_w(i-1))
  i = p_(2)
  f_n = IIF(i = 1, "", a_w(i-1))
  i = p_(3)
  f_a = IIF(i = 1, "", a_w(i-1))
  i = p_(4)
  f_f = IIF(i = 1, "", a_w(i-1))
  i = p_(5)
  f_q = IIF(i = 1, 0, a_w(i-1))
  IF TYPE("f_q") # "N"
    f_q = VAL(ALLTRIM(f_q))
  ENDIF
  f_p = PADL(ALLTRIM(f_p),4)
  SELECT T_ORD
  APPEND BLANK
  REPLACE PREFIX WITH f_p, ;
          NAME   WITH f_n, ;
          MODEL  WITH f_a, ;
          FUNC   WITH f_f, ;
          QNT    WITH f_q
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

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

SELECT T_ORD
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("QNT")+2

DO D_Wins WITH ln, wd, "蹍甃�矗 � ��玻オ罔� ��錢��", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", QNT:H=""   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        �諷�� ├ 蹣������ 筮爼�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.97 17:04:33 夕
PROCEDURE Term_F

PRIVATE mss, i, sz, pt

DIMENSION mss(3)

mss(1) = ""
mss(2) = CHR(0)+"   [ ��縲��━ ��錢�� ] [ �ム�ガ瘴 � ������ ] [ �瓮 ��＜ぅ� ]?"
mss(3) = ""

i = Out_Mess(5, "mss")
IF i = 2
  RETURN .F.
ENDIF

IF i = 3
  RETURN .T.
ENDIF

SELECT T_ORD
sz = HEADER()+RECNO()*RECSIZE()

pt = ""
DO WHILE .T.
  
  pt = Int_Dir(pt, "�襦ム�皀 ��Гカ か� ��錢��...", sz)
  
  IF EMPTY(pt)
    EXIT
  ENDIF
  
  IF File_O(pt+"ORD.DBF")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  � ��� 礒� メ碎 ���� � ��錢���! [ �����碎! ] [ ��茱� � む磽�� �メ皰 ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      LOOP
    ENDIF
    DELETE FILE (pt+"ORD.DBF")
  ENDIF
  
  GO TOP
  COPY TO (pt+"ORD.DBF")
  EXIT

ENDDO

RETURN .T.
