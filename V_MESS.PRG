*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� V_Mess       ��о�｀砒┴ ��むォ ��瓱�           16.01.98 11:43:28 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*� 蹍甃�矗 皀�痰�, �筮爼�キ���� � ※ぅ DBF-����� � �き━ 皀�痰��覓 ���ガ �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE V_Mess
PARAMETERS als, tit

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

PRIVATE ln, wd, lin, pages, point, nav_arr, ex, sel_sav, i, pic, p_sav
sel_sav = SELECT()
SELECT (als)
ln = MIN(WROWS("")-4,RECCOUNT()+4)
wd = FSIZE(FIELD(1))+4

pages = CEILING(RECCOUNT()/(ln-4))
point = 1
p_sav = 0
ex    = 1

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2

DO D_Win_N WITH ln, wd, tit
SCROLL WROWS()-2, 0, WROWS()-1, WCOLS()-1, 1
ln = ln-4

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  *
  *   ������碎 痰����聶
  *
  IF p_sav # point
    GO point
    @ 2, 2 CLEAR TO WROWS()-3, WCOLS()-3
    FOR i = 1 TO ln
      SCATTER TO lin
      @ 1+i, 2 SAY lin(1)
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  p_sav = point
  pic = "@*HT "
  i = 1
  IF point < RECCOUNT()-ln+1
    ON KEY LABEL PgDn DO Exit_N WITH "Next"
    pic = pic+"\ ��イ. PgDn ;"
    DIMENSION nav_arr(i)
    nav_arr(i) = "Next"
    i = i+1
  ENDIF
  IF point # 1
    ON KEY LABEL PgUp DO Exit_N WITH "Prev"
    pic = pic+"\ 踳�. PgUp ;"
    DIMENSION nav_arr(i)
    nav_arr(i) = "Prev"
    i = i+1
  ENDIF
  ON KEY LABEL UpArrow DO Exit_N WITH "Up"
  ON KEY LABEL DnArrow DO Exit_N WITH "Dn"
  DIMENSION nav_arr(i)
  nav_arr(i) = "Exit"
  pic = pic+"\? �諷�� Esc "
  what_do = ""
  @ WROWS()-1, 0 SAY SPACE(WCOLS())
  @ WROWS()-1, 2 GET ex PICTURE pic
  READ CYCLE
  ON KEY
  IF EMPTY(what_do)
    what_do = nav_arr(ex)
  ENDIF
  DO CASE
  CASE what_do = "Next"
    point = MIN(point+ln, RECCOUNT()-ln+1)
  CASE what_do = "Prev"
    point = MAX(point-ln,1)
  CASE what_do = "Up"
    point = MAX(point-1,1)
  CASE what_do = "Dn"
    point = MIN(point+1, RECCOUNT()-ln+1)
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
*-------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         蹍������ �÷�｀砒┴.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 16.01.98 12:36:16 夕
PROCEDURE Exit_N
PARAMETERS p

CLEAR READ
what_do = p
RETURN
