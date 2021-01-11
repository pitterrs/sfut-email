*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12/22/2020 at 14:23:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMALV_SFUT_EMAIL................................*
TABLES: ZMALV_SFUT_EMAIL, *ZMALV_SFUT_EMAIL. "view work areas
CONTROLS: TCTRL_ZMALV_SFUT_EMAIL
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMALV_SFUT_EMAIL. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMALV_SFUT_EMAIL.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMALV_SFUT_EMAIL_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMALV_SFUT_EMAIL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMALV_SFUT_EMAIL_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMALV_SFUT_EMAIL_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMALV_SFUT_EMAIL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMALV_SFUT_EMAIL_TOTAL.

*.........table declarations:.................................*
TABLES: T024E                          .
TABLES: ZMAL_SFUT_EMAIL                .
