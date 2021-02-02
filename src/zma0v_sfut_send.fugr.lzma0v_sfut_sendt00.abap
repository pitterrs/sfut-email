*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/29/2021 at 11:06:13
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMA0V_SFUT_SEND.................................*
TABLES: ZMA0V_SFUT_SEND, *ZMA0V_SFUT_SEND. "view work areas
CONTROLS: TCTRL_ZMA0V_SFUT_SEND
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMA0V_SFUT_SEND. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMA0V_SFUT_SEND.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMA0V_SFUT_SEND_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_SEND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_SEND_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMA0V_SFUT_SEND_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_SEND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_SEND_TOTAL.

*.........table declarations:.................................*
TABLES: T024                           .
TABLES: ZMA0_SFUT_SEND                 .
