*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍罐ゃ�� 甌縲��キ┤ �爬 √�ぅ � �牀������.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Entry_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - ━� �瓱�縲������ �キ� �襷襤�鉗ォ �牀������;
*    own_menu - ━� 甌＝癶キ���� �瓱�縲������ �キ�.
*
PRIVATE e_m, o_m, s

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(e_m)
  HIDE MENU (e_m)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

PUSH KEY CLEAR
IF .NOT. EMPTY(o_m)
  @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (o_m)
  s = "'"+o_m+"'"
  ON KEY LABEL F10 DO F10_Proc WITH &s
ENDIF

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍罐ゃ�� ��皋※��罔� �瓱�縲������ �キ�.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE F10_Proc
PARAMETER m_name

PUSH KEY CLEAR
ACTIVATE MENU (m_name)
DEACTIVATE MENU (m_name)
POP KEY

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍罐ゃ�� 〓瘁����←キ�� �爬 �諷�ぅ ├ �牀������.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Exit_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - ━� �瓱�縲������ �キ� �襷襤�鉗ォ �牀������;
*    own_menu - ━� 甌＝癶キ���� �瓱�縲������ �キ�.
*
PRIVATE e_m, o_m

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(o_m)
  DEACTIVATE MENU (o_m)
  RELEASE MENU (o_m) EXTENDED
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

POP KEY

IF .NOT. EMPTY(e_m)
  @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
  DEACTIVATE MENU (e_m)
  SHOW MENU (e_m)
ENDIF

RETURN

