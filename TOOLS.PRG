*
*   �����㬥�⠫쭠� �ணࠬ���...
*
PROCEDURE Tools
PRIVATE pth, prmp, prg, i, n, st
***************************************************************************
***************************************************************************
*      ��� ����� ��� ��� ᫥��� ������� ���� � �����㬥���...
*
pth = "d:\vasin\tools\"
***************************************************************************
DIMENSION p00(5), p01(5)
p00(1) = "��������� 䠩��"
p01(1) = "Fil_Tit"
p00(2) = "��������� ��楤���"
p01(2) = "Prg_Tit"
p00(3) = "����䨪��� ����� �. �."
p01(3) = "Md_Field"
p00(4) = "���४�� 蠡�����"
p01(4) = "Md_Shab WITH +'"+pth+"'"
p00(5) = "��⠢�� 蠡����"
p01(5) = "In_Shab WITH +'"+pth+"'"

DEFINE PAD _TOOLS OF _MSYSMENU PROMPT "�����㬥��" BEFORE _MSYSTEM
ON PAD _TOOLS OF _MSYSMENU ACTIVATE POPUP _TOOLS

DEFINE POPUP _TOOLS MARGIN RELATIVE SHADOW

FOR i = 1 TO ALEN(p00)

  st = "DO "+pth+p01(i)
  DEFINE BAR  i OF _TOOLS PROMPT p00(i)
  ON SELECTION BAR i OF _TOOLS &st

ENDFOR
