*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� ReadMenu     ��о�｀砒┴ ��むォ ��瓱�           20.03.97 21:26:00 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ��Бム硅��ガ �キ� � �襦���ガ �祗��.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE ReadMenu
PARAMETERS cd, arr_cd, arr_nm, tit
*
*  cd     - �����讚�� Л�腑�┘ ( ���! );
*  arr_cd - ��瘁│ ��ぎ�;
*  arr_nm - ��瘁│ ━キ;
*  tit    - ������〓� �キ�.

PRIVATE win_name, ln, wd, n, i
n = ALEN(arr_cd)
DIMENSION arr_cd(n), arr_nm(n)

i = ASCAN(arr_cd, cd)    && �����讚�� Л�腑�┘ 礫���皀��
win_name = "RM0000"      && ━� ����
PUSH KEY CLEAR
ln = MIN(n, WROWS("")-8)
wd = LEN(arr_nm(1))+4

DO D_Win_N WITH ln+3, wd, tit

ln = ln
DO WHILE .T.
  i = MIN(i, n)
  i = MAX(i, 1)
  @ 1, 1 MENU arr_nm, n, ln
  READ MENU TO i
  wd = READKEY() % 256
  IF wd = 15    && Cr
    i = arr_cd(i)
    EXIT
  ENDIF
  IF wd = 12    && Esc
    i = cd
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW (win_name)
POP KEY

RETURN i
