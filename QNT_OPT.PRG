*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Qnt_Opt     ��о�｀砒┴ ���┃� ���矗┘���                  �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  �痰���→� �カ����皰�諷 ����腑痰�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.10.2001 様�
PROCEDURE Qnt_Opt

PRIVATE s_sav

s_sav = SELECT()

IF sale_mode # "Mitracon"
  RETURN
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� �┃━��讚�� 痺��� �牀��Θ 99.999  �
*�                                   �
*� �┃━��讚�� ����腑痰〓    999999  �
*�                                   �
*� ���皚�痰�                 999999  �
*�                                   �
*� < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11, 39, "蹍��Θ �カ�━ ��皰�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@ 3, 3 SAY "�┃━��讚�� 痺��� �牀��Θ " GET sum_sal PICTURE "99.99"
@ 5, 3 SAY "�┃━��讚�� ����腑痰〓    " GET qnt_sal PICTURE "99999"
@ 7, 3 SAY "���皚�痰�                 " GET tmp_r PICTURE "99999"

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