*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Qnt_Opt     ���ࠡ��稪 ������ ����ਥ���                  �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                  ��⠭���� �������⮢�� �������.                     �
*�                                                                        �
*����������������������������������������������������������� 16.10.2001 �ͼ
PROCEDURE Qnt_Opt

PRIVATE s_sav

s_sav = SELECT()

IF sale_mode # "Mitracon"
  RETURN
ENDIF

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*
PRIVATE stor, temp, qnt_sal, sum_sal, ex, tmp, tmp_r
ex = 1
tmp = 0
stor = PADR(Get_Sys("QNT_OPT"), 40)
temp = SUBSTR(stor,1,10)
qnt_sal = IIF(EMPTY(temp), "0", temp)
qnt_sal = VAL(qnt_sal)

temp = SUBSTR(stor,12,10)
sum_sal = IIF(EMPTY(temp), "0", temp)
sum_sal = VAL(sum_sal)

temp = SUBSTR(stor,23,10)
tmp_r = IIF(EMPTY(temp), "0", temp)
tmp_r = VAL(tmp_r)

*�����������������������������������Ŀ
*� �������쭠� �㬬� �த��� 99.999  �
*�                                   �
*� �������쭮� ������⢮    999999  �
*�                                   �
*� ��⭮���                 999999  �
*�                                   �
*� < OK Ctrl-W > < �⪠������ Esc >  �
*�������������������������������������

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 11, 39, "�த��� ������ ��⮬"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
@ 3, 3 SAY "�������쭠� �㬬� �த��� " GET sum_sal PICTURE "99.99"
@ 5, 3 SAY "�������쭮� ������⢮    " GET qnt_sal PICTURE "99999"
@ 7, 3 SAY "��⭮���                 " GET tmp_r PICTURE "99999"

READ CYCLE
POP KEY
RELEASE WINDOW (win_name)

IF ex = 1

  DO Put_Sys WITH "QNT_OPT", STR(qnt_sal)+" "+STR(sum_sal)+" "+STR(tmp_r)
  
  USE(base_path+"STOCK") ALIAS STO_1A16 AGAIN ORDER TAG NAME_P
  SCAN
    WAIT WINDOW PREFIX+" "+NAME+" "+PRODUCER NOWAIT
    IF QNT_ROUND > 1
      REPLACE QNT_OPT WITH QNT_ROUND
    ELSE
      IF PRICE # 0
        IF sum_sal/PRICE <= qnt_sal
          REPLACE QNT_OPT WITH qnt_sal
        ELSE
          IF PRICE >= sum_sal
            tmp = qnt_sal
          ELSE
            tmp = ROUND(sum_sal/PRICE,0)
            IF tmp <= qnt_sal
              tmp = qnt_sal
            ELSE
              DO WHILE MOD(tmp,tmp_r) # 0
                tmp = tmp - 1
              ENDDO
            ENDIF
          ENDIF
          REPLACE QNT_OPT WITH tmp
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  USE
ENDIF

SELECT(s_sav)
RETURN