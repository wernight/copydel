DECLARE SUB Efface ()
DECLARE FUNCTION Liste (Cherche AS STRING) AS INTEGER
DECLARE SUB Arguments ()
DECLARE SUB Rajoute ()
DECLARE SUB Aide (Msg AS STRING)

CONST Titre = "CopyDel 1.12 (c) 1998 WBC par Werner BEROUX"

DIM SHARED Rep AS STRING, RepA AS STRING, RepB AS STRING, Args(1 TO 3) AS STRING
DIM SHARED Fichier AS STRING, NbrEff AS INTEGER, NbrErr AS INTEGER, Fait AS DOUBLE
DIM Lettre AS STRING * 1
  ON ERROR GOTO Erreur



  REM -----++++*** EFFECTUE LES TESTS ET PREND LES INFOS ***++++-----

  ' Donne les arguments de la ligne de commande
  CALL Arguments

  ' Aide
  IF Args(1) = "/?" OR Args(1) = "-?" OR Args(1) = "?" OR Args(1) = "" THEN Aide ""
  
  ' Donne les chemins
  RepA = Args(1)
  RepB = Args(2)

  ' Test
  IF RepA = "" OR RepB = "" THEN Aide "Erreur: ParamŠtre requis manquant."
  IF INSTR(RepB, ".") <> 0 OR INSTR(RepB, "*") <> 0 THEN Aide "Erreur: ne sp‚cifiez pas d'extensions pour le 2e r‚pertoire."

  ' Rajoute ce qu'il faut
  CALL Rajoute

  ' Si il ne trouve pas le chemin:
  IF DIR$(RepA) = "" THEN Aide "Erreur: chemin introuvable - " + RepA

  ' Affiche le d‚but
  PRINT Titre
  PRINT
  

  REM -----++++*** EFFACE ET AFFICHIER ***++++-----

  ' Efface
  DO
      ' Prend le prochain fichier
      Fichier = DIR$(RepB)
      FOR i& = 1 TO Fait
         Fichier = DIR$
      NEXT
      IF Fichier = "" THEN EXIT DO

      ' Si il trouve un doublon
      IF Liste(Fichier) = -1 THEN
         Fichier = Rep + Fichier

         ' Test si "/Y" et demand‚
         IF Args(3) = "/Y" THEN
            Efface
            GOTO Nouveau
         END IF
         
DemandeLettre:
         PRINT "Effacer " + Fichier + " (Oui/Non/Tout) ?"
         LOCATE CSRLIN - 1, LEN(Fichier) + 26
         INPUT "", Lettre
         SELECT CASE UCASE$(Lettre)
            CASE "O"
               Efface
            CASE "N"
               Fait = Fait + 1
            CASE "T"
               Args(3) = "/Y"
               Efface
            CASE ELSE
               GOTO DemandeLettre
         END SELECT
      ELSE
         Fait = Fait + 1
      END IF

Nouveau:
   LOOP

   ' Affiche le nombre d'erreurs
   IF NbrErr > 0 THEN PRINT STR$(NbrErr) + " erreur(s)!"

   ' Afficher le nombre de fichiers effac‚s
   IF NbrEff = 0 THEN
      PRINT "       Ancun doublon trouv‚ de " + RepA + " dans " + RepB
   ELSE
      PRINT "      " + STR$(NbrEff) + " fichier(s) effac‚(s)."
   END IF
   END

Erreur:
   PRINT "ERREUR FATALE: " + ERROR$(ERR) + "."
END

SUB Aide (Msg AS STRING)
  IF Msg <> "" THEN
    PRINT Msg
    END
  END IF

  PRINT Titre
  PRINT
  PRINT "Informations:"
  PRINT "  Tel et FAX               04 90 25 96 91"
  PRINT "  E-Mail                   WernerBeroux@Compuserve.com"
  PRINT "  Site Web                 http://www.alc.net/wbc"
  PRINT
  PRINT "COPYDEL <r‚pertoire test [fichiers]> <r‚pertoire d'effacement> [/Y]"
  PRINT "Options:"
  PRINT "  <r‚pertoire test>        R‚pertoire qui sert de r‚f‚rence."
  PRINT "  <r‚pertoire … effacer>   R‚pertoire dans lequel il efface les fichiers."
  PRINT "  [fichiers]               Sp‚cifications des fichiers … rechercher."
  PRINT "  [/Y]                     Ne demande pas de confirmation lors de l'effacemant."
  PRINT
  PRINT "COPYDEL Efface les doublons de fichier."

  END
END SUB

SUB Arguments ()
DIM Caractere AS STRING, NumArgum AS INTEGER

   FOR i% = 1 TO LEN(COMMAND$)
      Caractere = MID$(COMMAND$, i%, 1)
      IF Caractere <> " " AND Caractere <> CHR$(9) THEN  ' Teste pour v‚rifier si un caractŠre est blanc ou une tabulation
         IF NOT In THEN  ' Si nouvel argument
            IF NumArgum = 3 THEN EXIT FOR
            NumArgum = NumArgum + 1
            In = -1
         END IF

         Args(NumArgum) = Args(NumArgum) + Caractere   ' Ajoute le caractŠre … l'argument en cours
      ELSE
         In = 0
      END IF
   NEXT
END SUB

SUB Efface ()
   KILL Fichier
   ' Test si il est effac‚
   IF DIR$(Fichier) = "" THEN
      IF Args(3) = "/Y" THEN PRINT Fichier
      NbrEff = NbrEff + 1
   ELSE
      PRINT Fichier + " [ERREUR]"
      NbrErr = NbrErr + 1
      Fait = Fait + 1
   END IF
END SUB

STATIC FUNCTION Liste (Cherche AS STRING) AS INTEGER
DIM UneFois AS INTEGER, LenFichier AS INTEGER, Fichier(3000) AS STRING

   ' Cherche si c'est possible en Une Fois
   IF UneFois = 0 THEN
      Fichier(0) = DIR$(RepA)
      FOR i% = 1 TO 3000
         Fichier(i%) = DIR$
         IF Fichier(i%) = "" THEN
            LenFichier = i% - 1
            EXIT FOR
         END IF
      NEXT
      IF i% = 3000 THEN UneFois = 2 ELSE UneFois = 1
   END IF


   ' Chercher le fichier
   IF UneFois = 1 THEN
      FOR i% = 0 TO LenFichier
         IF Fichier(i%) = Cherche THEN
            Liste = -1
            EXIT FUNCTION
         END IF
      NEXT
      Liste = 0
   ELSEIF UneFois = 2 THEN
      DIM Fichier  AS STRING
      Fichier = DIR$(RepA)
      DO
         ' Si le fichier est trouv‚
         IF Cherche = Fichier THEN
            Liste = -1
            EXIT FUNCTION
         END IF

         Fichier = DIR$
      LOOP UNTIL Fichier = ""

      Liste = 0
   END IF
END FUNCTION

SUB Rajoute ()
   ' Rajoute "\" … la fin
   IF INSTR(RIGHT$(RepA, 4), ".") = 0 AND INSTR(RIGHT$(RepA, 4), "*") = 0 THEN
      IF NOT RIGHT$(RepA, 1) = "\" THEN RepA = RepA + "\"
      RepA = RepA + "*.*"
   END IF

   IF MID$(RepB, 2, 1) <> ":" OR LEN(RepB) <> 2 THEN IF RIGHT$(RepB, 1) <> "\" THEN RepB = RepB + "\"
   Rep = RepB
   RepB = RepB + "*.*"
END SUB

