*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 12/23/2020 at 07:35:02
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMALV_SFUT_SEND.................................*
FORM GET_DATA_ZMALV_SFUT_SEND.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZMAL_SFUT_SEND WHERE
(VIM_WHERETAB) .
    CLEAR ZMALV_SFUT_SEND .
ZMALV_SFUT_SEND-MANDT =
ZMAL_SFUT_SEND-MANDT .
ZMALV_SFUT_SEND-EKORG =
ZMAL_SFUT_SEND-EKORG .
ZMALV_SFUT_SEND-EMAIL =
ZMAL_SFUT_SEND-EMAIL .
    SELECT SINGLE * FROM ZMAL_SFUT_EMAIL WHERE
EKORG = ZMAL_SFUT_SEND-EKORG .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T024E WHERE
EKORG = ZMAL_SFUT_EMAIL-EKORG .
      IF SY-SUBRC EQ 0.
ZMALV_SFUT_SEND-EKOTX =
T024E-EKOTX .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZMALV_SFUT_SEND.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZMALV_SFUT_SEND .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZMALV_SFUT_SEND.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZMALV_SFUT_SEND-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZMAL_SFUT_SEND WHERE
  EKORG = ZMALV_SFUT_SEND-EKORG AND
  EMAIL = ZMALV_SFUT_SEND-EMAIL .
    IF SY-SUBRC = 0.
    DELETE ZMAL_SFUT_SEND .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZMAL_SFUT_SEND WHERE
  EKORG = ZMALV_SFUT_SEND-EKORG AND
  EMAIL = ZMALV_SFUT_SEND-EMAIL .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZMAL_SFUT_SEND.
    ENDIF.
ZMAL_SFUT_SEND-MANDT =
ZMALV_SFUT_SEND-MANDT .
ZMAL_SFUT_SEND-EKORG =
ZMALV_SFUT_SEND-EKORG .
ZMAL_SFUT_SEND-EMAIL =
ZMALV_SFUT_SEND-EMAIL .
    IF SY-SUBRC = 0.
    UPDATE ZMAL_SFUT_SEND .
    ELSE.
    INSERT ZMAL_SFUT_SEND .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZMALV_SFUT_SEND-UPD_FLAG,
STATUS_ZMALV_SFUT_SEND-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZMALV_SFUT_SEND.
  SELECT SINGLE * FROM ZMAL_SFUT_SEND WHERE
EKORG = ZMALV_SFUT_SEND-EKORG AND
EMAIL = ZMALV_SFUT_SEND-EMAIL .
ZMALV_SFUT_SEND-MANDT =
ZMAL_SFUT_SEND-MANDT .
ZMALV_SFUT_SEND-EKORG =
ZMAL_SFUT_SEND-EKORG .
ZMALV_SFUT_SEND-EMAIL =
ZMAL_SFUT_SEND-EMAIL .
    SELECT SINGLE * FROM ZMAL_SFUT_EMAIL WHERE
EKORG = ZMAL_SFUT_SEND-EKORG .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T024E WHERE
EKORG = ZMAL_SFUT_EMAIL-EKORG .
      IF SY-SUBRC EQ 0.
ZMALV_SFUT_SEND-EKOTX =
T024E-EKOTX .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMALV_SFUT_SEND-EKOTX .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMALV_SFUT_SEND-EKOTX .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZMALV_SFUT_SEND USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZMALV_SFUT_SEND-EKORG TO
ZMAL_SFUT_SEND-EKORG .
MOVE ZMALV_SFUT_SEND-EMAIL TO
ZMAL_SFUT_SEND-EMAIL .
MOVE ZMALV_SFUT_SEND-MANDT TO
ZMAL_SFUT_SEND-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZMAL_SFUT_SEND'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZMAL_SFUT_SEND TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZMAL_SFUT_SEND'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZMALV_SFUT_SEND USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZMAL_SFUT_SEND-MANDT =
ZMALV_SFUT_SEND-MANDT .
ZMAL_SFUT_SEND-EKORG =
ZMALV_SFUT_SEND-EKORG .
ZMAL_SFUT_SEND-EMAIL =
ZMALV_SFUT_SEND-EMAIL .
    SELECT SINGLE * FROM ZMAL_SFUT_EMAIL WHERE
EKORG = ZMAL_SFUT_SEND-EKORG .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T024E WHERE
EKORG = ZMAL_SFUT_EMAIL-EKORG .
      IF SY-SUBRC EQ 0.
ZMALV_SFUT_SEND-EKOTX =
T024E-EKOTX .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMALV_SFUT_SEND-EKOTX .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMALV_SFUT_SEND-EKOTX .
    ENDIF.
ENDFORM.