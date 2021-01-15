*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/15/2021 at 08:09:07
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMALV_SFUT_SEND.................................*
TABLES: ZMALV_SFUT_SEND, *ZMALV_SFUT_SEND. "view work areas
CONTROLS: TCTRL_ZMALV_SFUT_SEND
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMALV_SFUT_SEND. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMALV_SFUT_SEND.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMALV_SFUT_SEND_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMALV_SFUT_SEND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMALV_SFUT_SEND_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMALV_SFUT_SEND_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMALV_SFUT_SEND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMALV_SFUT_SEND_TOTAL.

*.........table declarations:.................................*
TABLES: T024E                          .
TABLES: ZMAL_SFUT_SEND                 .
