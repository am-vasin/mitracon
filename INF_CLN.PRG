*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     ���������� �� �������.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE INF_CLN
PRIVATE _file,_d_file,tmpMonth,tmpYEAR 

*DIMENSION M(12)
* M(1) = "����瑕"
* M(2) = "�ア����"
* M(3) = "��珥"
* M(4) = "��爛��"
* M(5) = "���"
* M(6) = "�遶�"
* M(7) = "�遨�"
* M(8) = "�■竅�"
* M(9) = "�キ硼÷�"
* M(10) = "ｪ硼÷�"
* M(11) = "��錙瑕"
* M(12) = "�オ�÷�"

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
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "�襦��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

***ｯ爛ぅ�錺� メ碎 �� 礒� ����� ����  
_file = base_path + "MOULD"
d_file = _file +".dbf"
 
 IF NOT FILE_O (d_file)
	DO MOULD WITH MONTH(DATE()),YEAR(DATE())
 ENDIF
 
 IF NOT USED("MOULD")
 SELECT 0
 USE (base_path+"MOULD")
 ENDIF

 IF NOT USED("DUMMY")
 DO USE_DUMMY
 ENDIF

 SELECT MOULD
 tmpMonth = MOULD.MONTH
 tmpYEAR = MOULD.YEAR


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, f_name, cl
*ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
ln = 20   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd =15

DO D_Wins WITH ln, wd, " ��゛���", 0, 0
what_do = "list"
*@ 1, 2 SAY SPACE(FSIZE("MONTH")+FSIZE("YEAR")+FSIZE("FLAG"))+" ��゛���"
@ 1, 2 SAY PADR("�メ閹    ",(FSIZE("MONTH")+FSIZE("YEAR")+FSIZE("FLAG")))+" ��� �癶"
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""

DO WHILE .T.
  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

	statys_type = 3    
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

 SELECT MOULD
*
*    BROWSE - �キ�
*
    BROWSE FIELDS M00 = Month_R(MONTH) :8 :W=.F.:H="",     ;
           YEAR		:H = "���", ;
           F_N = IIF(EMPTY(FLAG),  " ", "�"):H="" ,;
           DUMMY.F FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")  &&COLOR (cl)
    ON KEY

    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"      && �襦�� �メ閹� か� �牀甃�矗�
*    DO LS_TABLE 
   what_do = "list"
	EXIT
  OTHERWISE
	SEEK STR(tmpYEAR,4)+STR(tmpMonth,2)

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW(win_name+"_I")
RELEASE WINDOW(win_name+"_E")
RELEASE WINDOW(win_name+"_M")

RETURN

