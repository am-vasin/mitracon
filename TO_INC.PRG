*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           ��腮甄�碎 ぅ�譯�!                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 30.10.97 13:36:38 夕
PROCEDURE To_Inc

PARAMETERS cd, ps_cd, q, pr, ttl, unt, ye, mn, nt

*
* cd    - ��� 皋�� ��腮甄キ��;
* ps_cd - ��� 甌矗磴�┴�;
* q     - ����腑痰〓;
* pr    - 罐��;
* ttl   - 痺���;
* unt   - イ┃�肓;
* ye    - ���;
* mn    - �メ閹;
* nt    - �爬�ョ��┘.
*

PRIVATE s_sav, pr0, ttl0, d_n, d_d, d_t
pr0  = Crpt_1(pr, 10,2)
ttl0 = Crpt_1(ttl,10,2)

s_sav = SELECT()
SELECT 0
USE (base_path+"SLR_MOVE") ORDER TAG DOC_NUM ALIAS W98765 AGAIN
d_d = DATE()
d_t = TIME()
GO BOTTOM
IF BOF() .OR. EOF()
  d_n = 1
ELSE
  IF YEAR(d_d) # YEAR(DOC_DATE)
    d_n = 1
  ELSE
    d_n = VAL(ALLTRIM(DOC_NUM))+1
  ENDIF
ENDIF
d_n = STR(d_n,7)

APPEND BLANK
REPLACE	DOC_NUM		WITH d_n,	;
		DOC_DATE	WITH d_d,	;
		DOC_TIME	WITH d_t,	;
		CODE		WITH cd,	;
		PS_CODE		WITH ps_cd,	;
		QNT			WITH q,		;
		PRICE		WITH pr0,	;
		TOTAL		WITH ttl0,	;
		UNIT		WITH unt,	;
		YEAR		WITH ye,	;
		MONTH		WITH mn,	;
		NOTE		WITH nt,	;
		WHO			WITH user,	;
		WHERE		WITH arm

USE (base_path+"PS_STAT") AGAIN ALIAS PST_1411 ORDER TAG CODE
SEEK ps_cd
ttl0 = Crpt_2(REST)+ttl
REPLACE REST WITH Crpt_1(ttl0,10,2)

USE

SELECT (s_sav)
RETURN
