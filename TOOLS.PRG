*
*   Инструментальная программка...
*
PROCEDURE Tools
PRIVATE pth, prmp, prg, i, n, st
***************************************************************************
***************************************************************************
*      Вот здесь вот Вам следует записать путь к инструменту...
*
pth = "d:\vasin\tools\"
***************************************************************************
DIMENSION p00(5), p01(5)
p00(1) = "Заголовок файла"
p01(1) = "Fil_Tit"
p00(2) = "Заголовок процедуры"
p01(2) = "Prg_Tit"
p00(3) = "Модификация полей Б. Д."
p01(3) = "Md_Field"
p00(4) = "Коррекция шаблонов"
p01(4) = "Md_Shab WITH +'"+pth+"'"
p00(5) = "Вставка шаблона"
p01(5) = "In_Shab WITH +'"+pth+"'"

DEFINE PAD _TOOLS OF _MSYSMENU PROMPT "Инструмент" BEFORE _MSYSTEM
ON PAD _TOOLS OF _MSYSMENU ACTIVATE POPUP _TOOLS

DEFINE POPUP _TOOLS MARGIN RELATIVE SHADOW

FOR i = 1 TO ALEN(p00)

  st = "DO "+pth+p01(i)
  DEFINE BAR  i OF _TOOLS PROMPT p00(i)
  ON SELECTION BAR i OF _TOOLS &st

ENDFOR
