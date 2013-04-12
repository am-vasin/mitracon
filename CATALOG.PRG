SET SYSMENU OFF
*SET SYSMENU AUTOMATIC
SET COLOR TO N/W
CLEAR

DEFINE MENU MainMenu IN SCREEN COLOR SCHEME 3 &&N+/W,N/W,,,W+/B, N+/N,W+/W,,W+/B,W+/B

DEFINE PAD Catalog OF MainMenu PROMPT '\<Обновление каталога' 
ON  SELECTION PAD Catalog OF MainMenu DO NewCatal
  
DEFINE PAD Rolik OF MainMenu PROMPT '\<Реклама' MESSAGE 'Информация и реклама'  
ON  SELECTION PAD Rolik OF MainMenu DO Reclama 

DEFINE PAD Spravka OF MainMenu PROMPT '\<Справки' MESSAGE 'Справка по номенклатуре'
ON SELECTION PAD Spravka OF MainMenu DO Spravka

DEFINE PAD Zayavka OF MainMenu PROMPT '\<Заявка' MESSAGE 'Формирование заявки на поставку'
ON SELECTION PAD ZAYAVKA OF MainMenu DO FormOrder

DEFINE PAD Exit OF MainMenu PROMPT '\<Выход'
ON SELECTION PAD Exit OF MainMenu DO ExitProc

? "!!!!!!!!!!!!!!!!!!!!"

ACTIVATE MENU MainMenu
? "??????????????????"
show menu mainmenu
? "1111111111111111111111"
**********************************

**********************************
PROCEDURE NewCatal
RETURN
**********************************

**********************************
PROCEDURE Reclama
RETURN
**********************************

**********************************
PROCEDURE Spravka
RETURN
**********************************

**********************************
PROCEDURE FormOrder
RETURN
**********************************

**********************************
PROCEDURE ExitProc
CLOSE DATA
CLEAR WIND
ON KEY
SET COLOR TO
DEACTIVATE MENU MainMenu
CANCEL
**********************************