*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rem_Base     ��о�｀砒┴ ��むォ ��瓱�           14.01.98 16:45:56 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 �腮痰�� ��щ � 〓瘁����←キ┘ �������.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rem_Base

SET DATE GERMAN
SET DELETED ON
SET TALK OFF

base_path = "D:\ROZN\base\"
sys_char = "B"

USE (base_path+"PARMS")

CLEAR
@ 10, 10 SAY "�痰���←│�ガ 痰�珥�≪� ��矣" GET DAT_START
READ
CLEAR

USE

IF READKEY() % 256 = 12
  RETURN
ENDIF

WAIT WINDOW " 蹍��Θ, ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Sal
WAIT WINDOW " 踸絎�, ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Inc
WAIT WINDOW " �ク�Ж�, ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Acc
WAIT WINDOW " ��｀腮� �メ��, ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Arm
WAIT WINDOW " ��瘁�, ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Kas
WAIT WINDOW " ����腮�, ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Nal
WAIT WINDOW " ����錺�! ������, ��む鈬�, ANY KEY! " NOWAIT
DO Rem_Del

CLEAR

@ 10, 10 SAY "�瓮!"
