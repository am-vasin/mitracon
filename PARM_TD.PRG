*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Подготовка параметров передачи.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Parm_TD
PARAMETER id_c    && Ключ - тип обмена+код корреспондента

*
*   Типы обменов
*  SM   - передача на основной склад
*  SF   - передача в филиал
*  RM   - прием с основного склада
*  RF   - прием из филиала
*

PRIVATE i, sw       && Хлам!
*
*  Снаружи д. б. определены:
*    s_date - начальная дата ( только для передачи ), при входе - любая;
*    t_num  - номер пакета ( только для передачи ), при входе - любой.
*

USE TR_LOG ORDER TAG TR_NUM DESCENDING ALIAS TMP000 AGAIN
IF SEEK(id_c)                 && Ищем последнюю (!) передачу!
  t_num = TR_NUM+1
  sw = .T.
  FOR i = 1 TO n_over
    SKIP
    IF EOF() .OR. TR_CODE+ID_CORR # id_c  && Не удается отмотать на n_over пакетов...
      sw = .F.
      EXIT
    ENDIF
  ENDFOR
  IF sw
    s_date = TR_DATE
  ELSE
    USE PARMS ALIAS TMP000 AGAIN
    s_date = DAT_START
  ENDIF
ELSE
  USE PARMS ALIAS TMP000 AGAIN
  s_date = DAT_START
  t_num = 1
ENDIF
USE

RETURN
