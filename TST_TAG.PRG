*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Tst_Tag      ��о�｀砒┴ ��むォ ��瓱�           20.11.97 16:46:31 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           蹍▲爲� ┃ぅ���.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Tst_Tag
PARAMETERS full_name, tag_name

PRIVATE n_ord, n_free, com_str, key_val, fltr
PRIVATE win_name, t_name, t_numb, i, err, tit

com_str = "USE "+full_name+" ORDER TAG "+tag_name+" ALIAS TST_TAG AGAIN"
&com_str
win_name = PROGRAM()

t_name = ""
t_numb = 0
IF TYPE("tag_name") = "C"
  t_name = UPPER(ALLTRIM(tag_name))
  i = 1
  DO WHILE .T.
    IF TAG(i) == t_name
      t_numb = i
      EXIT
    ENDIF
    IF EMPTY(TAG(i))
      EXIT
    ENDIF
    i = i+1
  ENDDO
ENDIF
IF TYPE("tag_name") = "N"
  t_numb = tag_name
  t_name = TAG(t_numb)
ENDIF
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰-皰 �� ���閧��, ����� TAG �牀▲瑙ガ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���� �竅�, �牀▲爲� ������ ��矗磴�キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

com_str = SYS(14, t_numb)
fltr    = SYS(2021, t_numb)
key_val = EVALUATE(com_str)
n_ord  = 0
n_free = 0

tit = RAT("\", full_name)
IF tit # 0
  tit = SUBSTR(full_name,tit+1)
ELSE
  tit = IIF(SUBSTR(full_name, 2, 1) = ":", SUBSTR(full_name, 3), full_name)
ENDIF
tit = "���� "+UPPER(ALLTRIM(tit))+", TAG "+UPPER(ALLTRIM(t_name))

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                   �
*�  ���ム ����瓱 � 祚�瑙ぎ腑���� �����   臼臼臼臼臼  �
*�                                                   �
*�  ���ム ����瓱 � �ャ��瑙ぎ腑���� ����� 臼臼臼臼臼  �
*�                                                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Win_N WITH 8, 55, tit
@ 3, 4 SAY "���ム ����瓱 � 祚�瑙ぎ腑���� �����"
@ 5, 4 SAY "���ム ����瓱 � �ャ��瑙ぎ腑���� �����"

err = ""
SCAN
  i = EVALUATE(com_str)
  IF i < key_val
    err = "����瘡 N "+ALLTRIM(STR(RECNO()))+" ��珮茱�┘ ��甄イ���皀�讚�痰�!"
    EXIT
  ENDIF
  IF n_ord > RECCOUNT()
    err = "��瘍����� ����瓮� ｀�跏�, 腑� � ����� 〓�♂�!"
    EXIT
  ENDIF
  key_val = i
  n_ord = n_ord+1
  @ 3, 41 SAY STR(n_ord, 10) COLOR SCHEME 1
ENDSCAN

IF EMPTY(err)
  SET ORDER TO
  SET FILTER TO &fltr
  SCAN
    n_free = n_free+1
    @ 5, 41 SAY STR(n_free, 10) COLOR SCHEME 1
  ENDSCAN
  IF n_free # n_ord
    err = "�� 甌�����鈞 ����腑痰�� �爬 ��め腑��� � ┃ぅ�甌� � ．� �ィ�!"
  ENDIF
ENDIF

USE

IF .NOT. EMPTY(err)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+err
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ぅ�� � ��瑙お�!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF

RELEASE WINDOW (win_name)

RETURN
