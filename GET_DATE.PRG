*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Get_Date     ��о�｀砒┴ ��むォ ��瓱�           23.09.98 16:09:11 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                               ���キ��瑕                                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Get_Date
PARAMETER s_date

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


*敖陳陳陳陳陳陳陳陳陳陳陳�
*� ��  00 00 00 00 00 00 � �キ� !, !
*� ��  00 00 00 00 00 00 � �イカ� !, !
*� ��  00 00 00 00 00 00 � �メ閹 Tab, Shift-Tab
*� ��  00 00 00 00 00 00 � �襦��碎 Enter
*� ��  00 00 00 00 00 00 � �皖���碎瘴 Esc
*� ��  00 00 00 00 00 00 � �ィ�き� Home
*� ��  00 00 00 00 00 00 � �腮痰�碎 蹍．�
*青陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE f_year, f_month, date_w, day_pos, m_list, s, m_r, m_c, i
DIMENSION day_pos(31, 2), m_list(12)
m_list( 1) = "����瑕"
m_list( 2) = "�ア����"
m_list( 3) = "��珥"
m_list( 4) = "��爛��"
m_list( 5) = "���"
m_list( 6) = "�遶�"
m_list( 7) = "�遨�"
m_list( 8) = "�■竅�"
m_list( 9) = "�キ硼÷�"
m_list(10) = "ｪ硼÷�"
m_list(11) = "��錙瑕"
m_list(12) = "�オ�÷�"

date_w = IIF(EMPTY(s_date), DATE(), s_date)
f_year = YEAR(date_w)
f_month= MONTH(date_w)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 49, ""
@ 0, 1 CLEAR TO WROWS()-1, WCOLS()-1
DO Sun_Bord WITH 1, 1, 9, 25
@ 2, 3 SAY "��"
@ 3, 3 SAY "��"
@ 4, 3 SAY "��"
@ 5, 3 SAY "��"
@ 6, 3 SAY "��"
@ 7, 3 SAY "��" COLOR (SCHEME(14,2))
@ 8, 3 SAY "��" COLOR (SCHEME(14,2))
@ 2, 27 SAY "�キ�  ,"
@ 2, 32 SAY CHR(24) COLOR (SCHEME(14,2))
@ 2, 35 SAY CHR(25) COLOR (SCHEME(14,2))
@ 3, 27 SAY "�イカ�  ,"
@ 3, 34 SAY CHR(26) COLOR (SCHEME(14,2))
@ 3, 37 SAY CHR(27) COLOR (SCHEME(14,2))
@ 4, 27 SAY "�メ閹 Tab, Shit-Tab"
@ 4, 33 SAY "Tab" COLOR (SCHEME(14,2))
@ 4, 38 SAY "Shift-Tab" COLOR (SCHEME(14,2))
@ 5, 27 SAY "�襦��碎 Enter"
@ 5, 35 SAY "Enter" COLOR (SCHEME(14,2))
@ 6, 27 SAY "�皖���碎瘴 Esc"
@ 6, 38 SAY "Esc" COLOR (SCHEME(14,2))
@ 7, 27 SAY "�ィ�き� Home"
@ 7, 35 SAY "Home" COLOR (SCHEME(14,2))
@ 8, 27 SAY "�腮痰�碎 蹍．�"
@ 8, 36 SAY "蹍．�" COLOR (SCHEME(14,2))
DO Fil_Days
DO WHILE .T.
  s = DAY(date_w)
  @ day_pos(s,1), day_pos(s,2) FILL TO day_pos(s,1), day_pos(s,2)+1 ;
       COLOR SCHEME 1
  s = INKEY(0,"HM")
  @ 2, 7 FILL TO 8, 24 COLOR (SCHEME(1,2))
  IF s = 151  && �│�皚��! ...┼� � - ����ア�碎...
    s = 0
    m_c = MCOL()
    m_r = MROW()
    IF m_c >= 0 .AND. m_r >= 0
      FOR i = 1 TO 31
        IF m_r = day_pos(i,1) .AND. BETWEEN(m_c, day_pos(i,2), day_pos(i,2)+1)
          date_w = CTOD(STR(i,2)+"."+ALLTRIM(STR(f_month))+"."+ALLTRIM(STR(f_year)))
          s = 13
          EXIT
        ENDIF
      ENDFOR
      DO CASE
      CASE m_r = 2 .AND. m_c = 32  && �矗カ�� □ム�
        s = 5
      CASE m_r = 2 .AND. m_c = 35  && �矗カ�� ↓├
        s = 24
      CASE m_r = 3 .AND. m_c = 34  && �矗カ�� ����〓
        s = 4
      CASE m_r = 3 .AND. m_c = 37  && �矗カ�� ←ア�
        s = 19
      CASE m_r = 4 .AND. BETWEEN (m_c, 33,  35)  && Tab
        s = 9
      CASE m_r = 4 .AND. BETWEEN (m_c, 38,  46)  && Shift-Tab
        s = 15
      CASE m_r = 5 .AND. BETWEEN (m_c, 35,  39)  && Enter
        s = 13
      CASE m_r = 6 .AND. BETWEEN (m_c, 38,  40)  && Esc
        s = 27
      CASE m_r = 7 .AND. BETWEEN (m_c, 35,  38)  && Home
        s = 1
      CASE m_r = 8 .AND. BETWEEN (m_c, 36,  41)  && Space
        s = 32
      ENDCASE
    ENDIF
  ENDIF
  DO CASE
  CASE s = 5     && �矗カ�� □ム�
    date_w = date_w-1
  CASE s = 24    && �矗カ�� ↓├
    date_w = date_w+1
  CASE s = 4     && �矗カ�� ����〓
    date_w = date_w+7
  CASE s = 19    && �矗カ�� ←ア�
    date_w = date_w-7
  CASE s = 9     && Tab
    date_w = GOMONTH(date_w,1)
  CASE s = 15    && Shift+Tab
    date_w = GOMONTH(date_w,-1)
  CASE s = 13    && Enter
    EXIT
  CASE s = 27    && Esc
    date_w = s_date
    EXIT
  CASE s = 1     && Home
    date_w = DATE()
  CASE s = 32    && 蹍．�
    date_w = {}
    EXIT
  OTHERWISE
    LOOP
  ENDCASE
  IF f_year # YEAR(date_w) .OR. f_month # MONTH(date_w)
    DO Fil_Days
  ENDIF

ENDDO

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN date_w

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                  ��甎�����ガ き� �メ閹� � ���キ��爛.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.09.98 16:54:59 夕
PROCEDURE Fil_Days

PRIVATE mn0, dt0, i, j, k

@ 2, 7 CLEAR TO 8, 24
f_year = YEAR(date_w)
f_month= MONTH(date_w)
FOR i = 1 TO 31
  day_pos(i,1) = -1
  day_pos(i,2) = -1
ENDFOR
@ 0, 0 SAY PADC(m_list(f_month)+STR(f_year,5), WCOLS()) COLOR SCHEME 5
dt0 = CTOD("01."+ALLTRIM(STR(f_month))+"."+ALLTRIM(STR(f_year)))
i = 7
j = DOW(dt0)
j = IIF(j = 1, 7, j-1)+1
k = 1
DO WHILE MONTH(dt0) = f_month
  day_pos(k,1) = j
  day_pos(k,2) = i
  @ j, i SAY STR(k,2)
  dt0 = dt0+1
  k = k+1
  j = j+1
  IF j > 8
    j = 2
    i = i+3
  ENDIF
ENDDO

RETURN
