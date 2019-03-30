*&---------------------------------------------------------------------*
*& Report  ZMCIBUCH_HL7_CREATE_HL7_MSG
*&---------------------------------------------------------------------*
*& Beispiel zur Erzeugung einer neuen HL7-Nachricht
*&---------------------------------------------------------------------*
*& Dieses Programm demonstriert den Aufbau einer HL7-Nachricht mit
*& Hilfe der Klasse ZCL_MCIBUCH_HL7_CREATE_DEMO.
*&---------------------------------------------------------------------*
*& Dieses Programm ist eine nicht gewartete Beispiel-Implementierung
*& zur Ergänzung des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9.
*& Nutzung auf eigene Gefahr. Beachten Sie die Hinweise in
*& Kapitel 5.2.4
*&---------------------------------------------------------------------*
*& THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC
*& LICENSE. ANY USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM
*& CONSTITUTES RECIPIENT'S ACCEPTANCE OF THIS AGREEMENT.
*&---------------------------------------------------------------------*
REPORT zmcibuch_hl7_create_hl7_msg.

PARAMETERS p_einri TYPE einri
  OBLIGATORY
  MEMORY ID ein
  MATCHCODE OBJECT f4_ish_einri.

PARAMETERS p_falnr TYPE falnr
  OBLIGATORY
  MEMORY ID fal
  MATCHCODE OBJECT ish_fall_with_popup.

START-OF-SELECTION.
  NEW zcl_mcibuch_hl7_create_demo( )->execute(
      i_institution_id = p_einri
      i_case_id        = p_falnr
  ).
