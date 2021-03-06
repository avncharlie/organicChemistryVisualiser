﻿Imports System.Text.RegularExpressions

Module IUPACParser

    ' --- functional groups ---
    Public Structure FunctionalGroupChild
        Public branches As ConnectingBranches
        Public bondType As Integer
        Public representor As String
        Public colour As String
        Public children As FunctionalGroupChild()
    End Structure

    Public Structure FunctionalGroupDefinition
        Public type As String
        Public XML As XElement
        Public children As FunctionalGroupChild()
    End Structure

    ' --- tokenisation ---
    Public Structure TokenDefinition
        Public name As String
        Public regex As String
        Public isBooleanRegex As Boolean
        Public isSpecial As Boolean
        Public contained As String()
        Public followers As String()
        Public XML As XElement
    End Structure

    Public Structure Token
        Public type As String
        Public value As String
    End Structure

    ' --- parsing  ---
    Public Structure ConnectingBranches
        Public takenBranches As Integer()
    End Structure

    Public Structure ASTFunctionalGroup
        Public type As String
        Public locants As String()
    End Structure

    Public Structure ASTAlkaneBase
        Public isError As Boolean
        Public errorMessage As String
        Public isMainChain As Boolean
        Public locants As String()
        Public isCyclical As Boolean
        Public length As Integer
        Public branches As ConnectingBranches()
        Public simpleSubstituents As ASTFunctionalGroup()
        Public complexSubstituents As ASTAlkaneBase()
    End Structure

    Public Structure ASTRoot
        Public organicName As String
        Public compoundTree As ASTAlkaneBase
    End Structure

    ''' <summary>
    ''' replaces whatever replacements need to be made and converts to lowercase
    ''' </summary>
    ''' <param name="organicName"></param>
    ''' <param name="replacementsRoot"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function preprocessing(ByVal organicName As String, ByVal replacementsRoot As XElement)
        organicName = organicName.ToLower()
        For Each replacement In replacementsRoot.Elements("replacement")
            organicName = System.Text.RegularExpressions.Regex.Replace(organicName, replacement.Attribute("name").Value, replacement.Value)
        Next
        Return organicName
    End Function

    ''' <summary>
    ''' Given an AST as an ASTAlkaneBase, return a copy of the AST. Fully copies all arrays present in the original AST so there
    ''' are no unwanted links between the original and the copy.
    ''' </summary>
    ''' <param name="origAST">The AST to copy</param>
    ''' <returns>A copy of origAST</returns>
    ''' <remarks></remarks>
    Public Function copyAST(ByVal origAST As ASTAlkaneBase) As ASTAlkaneBase
        Dim copied As IUPACParser.ASTAlkaneBase
        copied.branches = {}
        copied.complexSubstituents = {}
        copied.errorMessage = origAST.errorMessage
        copied.isCyclical = origAST.isCyclical
        copied.isError = origAST.isError
        copied.isMainChain = origAST.isMainChain
        copied.length = origAST.length
        copied.locants = {}
        copied.simpleSubstituents = {}

        ' copy arrays so they are not linked
        ReDim copied.branches(origAST.branches.Length - 1)
        Array.Copy(origAST.branches, copied.branches, origAST.branches.Length)

        ReDim copied.complexSubstituents(origAST.complexSubstituents.Length - 1)
        Array.Copy(origAST.complexSubstituents, copied.complexSubstituents, origAST.complexSubstituents.Length)

        ReDim copied.locants(origAST.locants.Length - 1)
        Array.Copy(origAST.locants, copied.locants, origAST.locants.Length)

        ReDim copied.simpleSubstituents(origAST.simpleSubstituents.Length - 1)
        Array.Copy(origAST.simpleSubstituents, copied.simpleSubstituents, origAST.simpleSubstituents.Length)

        Return copied
    End Function

    ''' <summary>
    ''' Given a the name of an organic compound or substituent fragment, an array of tokens representing this name, an array of 
    ''' token definitions, an indication if the given name contains the main chain of the compound and the locants of the chain
    ''' if it isn't, return an ASTAlkaneBase structure representing this object. Deals with nested main chains (complex substituents)
    ''' recursively. Doesn't initialise the branches variable, used while rendering.
    ''' 1000 line function justification: In reality, parsing tokens and generating an AST  would be done using a finite state 
    ''' automata. This function was implemented without a finite state automata due to the author's lack of knowledge regarding
    ''' its implementation. This resulted in a large amount of code with many conditional and repetition control structures used
    ''' in this function. While the function is a 1000 lines long, it still only does one logical task - parse a list of tokens 
    ''' into an AST.
    ''' </summary>
    ''' <param name="organicNameString">
    ''' A string containing the name of the organic compound (used in error messages). In the case of a complex substituent, the
    ''' complex substituent fragment is expected (e.g but-1-enyl)
    ''' </param>
    ''' <param name="nameTokens">An array of Token objects representing the name to be parsed</param>
    ''' <param name="isMainChain">A boolean that is true if the given name contains the main chain, false if not</param>
    ''' <param name="locants">
    ''' An array containing the locants of the given complex substituent if not isMainChain
    ''' e.g. for 2-butyl, locants = {2}
    ''' If isMainChain, empty list should be given (e.g. {})
    ''' </param>
    ''' <param name="tokenDefinitions">An array of (generated) TokenDefinitions</param>
    ''' <param name="functionalGroupDefinitions">An array of (generated) FunctionalGroupDefinitions</param>
    ''' <returns>
    ''' If valid name, return an ASTAlkaneBase structure representing name
    ''' If invalid name, return an ASTAlkaneBase structure with the isError boolean flag set as True with a generated error message
    ''' present in the errorMessage variable
    ''' </returns>
    ''' <remarks>Should be called by the generateAST function, not by the user</remarks>
    Private Function generateASTAlkaneBase(ByVal organicNameString As String, ByVal nameTokens As Token(), ByVal isMainChain As Boolean, ByVal locants As String(), ByRef tokenDefinitions As TokenDefinition(), ByRef functionalGroupDefinitions As FunctionalGroupDefinition()) As ASTAlkaneBase
        ' OVERVIEW -
        ' key: 
        '   token1 + token2 means token1 is followed by token 2
        '   [token] means that token is optional
        '   [token2 + [token1]] means token 1 is optional, but will only appear if token 2 appears
        '   (token1 | token2) means one of either token1 or token2
        ' definitions:
        '   simple substituent: locantGroup + hyphen + [groupRepeater] + functionalGroupSubstituent
        '   main chain base: [cyclicalIndicator] + multiplier + [hyphen + locantGroup + hyphen + [groupRepeater]] + aneEneYneSuffix + [hyphen + locantGroup + hyphen + [groupRepeater] + (functionalGroupMainChainSuffix | aneEneYneSuffix)]*
        '       at the moment names like 3-hexene where the locant comes first in the main chain are not supported as they are ambiguous; for example, how would you specify hex-3-en-5-yne in this form?
        '   complex substituent base: main chain base + substituentEnd
        '       to parse, remove substituent base end and recurse. also put in alkAneEneSuffix if not there, e.g methyl -> meth -> (realise there is a multiplier without an alkAneEneSuffix and add it) -> methane
        '       complex substituents can be in the form: locantGroup + hyphen + openBracket + complex substituent + closeBracket OR locantGroup + hyphen + complex substituent
        '       to differentiate from simple substituent - first make sure its not the main chain and then if any substituent has a multiplier in it, that group is automatically a complex substituent
        ' errors:
        '   return in the form "ERROR: <error message>"

        Dim alkaneBase As ASTAlkaneBase
        alkaneBase.isError = False
        alkaneBase.errorMessage = ""
        alkaneBase.isMainChain = isMainChain
        alkaneBase.isCyclical = False
        alkaneBase.locants = locants
        alkaneBase.length = -1
        alkaneBase.branches = {}
        alkaneBase.simpleSubstituents = {}
        alkaneBase.complexSubstituents = {}

        ' indicates error position in organicName ("         ^ ")
        Dim errorPointer As String

        ' indicates how many characters the substituents are (used to offset errorPonter when displaying an error for the main chain)
        Dim substituentOffset As Integer

        ' 1. seperate the tokens into two groups - main chain and substituents
        Dim mainChainTokens As Token()
        Dim substituentTokens As Token()
        mainChainTokens = {}
        substituentTokens = {}

        Dim passedMainChain As Boolean
        passedMainChain = False

        Dim justPassedMainChain As Boolean
        justPassedMainChain = False

        Dim locantOffset As Integer
        Dim locantGroupArray As String()

        Dim groupRepeaterNumber As Integer

        For index = nameTokens.Length - 1 To 0 Step -1
            If Not passedMainChain Then
                ' still going through main chain tokens
                ReDim Preserve mainChainTokens(mainChainTokens.Length)
                mainChainTokens(mainChainTokens.Length - 1) = nameTokens(index)
                If nameTokens(index).type = "multiplier" Then
                    ' reached start of main chain
                    passedMainChain = True
                    justPassedMainChain = True
                End If
            Else
                ' going through substituent tokens, but if first token is a cyclical indicator than add that to main chain tokens list
                If justPassedMainChain Then
                    justPassedMainChain = False
                    If nameTokens(index).type = "cyclicalIndicator" Then
                        ReDim Preserve mainChainTokens(mainChainTokens.Length)
                        mainChainTokens(mainChainTokens.Length - 1) = nameTokens(index)
                    Else
                        ReDim Preserve substituentTokens(substituentTokens.Length)
                        substituentTokens(substituentTokens.Length - 1) = nameTokens(index)
                    End If
                Else
                    ReDim Preserve substituentTokens(substituentTokens.Length)
                    substituentTokens(substituentTokens.Length - 1) = nameTokens(index)
                End If
            End If
        Next

        If Not passedMainChain Then
            ' throw error - this means there is no multiplier in the main chain
            alkaneBase.isError = True
            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                & organicNameString & Environment.NewLine _
                & "^" & Environment.NewLine _
                & "malformed main chain, no multiplier (meth, eth, prop, etc...) present"
            Return alkaneBase
        End If

        ' reverse both mainChainTokens and substituentTokens as they were filled in backwards
        Array.Reverse(mainChainTokens)
        Array.Reverse(substituentTokens)

        ' calculate substituent offset
        substituentOffset = 0
        For sToken = 0 To substituentTokens.Length - 1
            For cToken = 0 To substituentTokens(sToken).value.Length - 1
                substituentOffset = substituentOffset + 1
            Next
        Next
        substituentOffset = substituentOffset - 1

        ' 2. parse main chain
        ' main chain base: [cyclicalIndicator] + multiplier + [hyphen + locantGroup + hyphen + [groupRepeater]] + aneEneYneSuffix + [hyphen + locantGroup + hyphen + [groupRepeater] + (functionalGroupMainChainSuffix | aneEneYneSuffix)]*
        '                                        ^ length               ^ locants of suffix if ene or yne         ^ only important if ene or yne  ^ locants of coming substituent      ^ substituent

        Dim counter As Integer
        counter = 0

        If mainChainTokens(counter).type = "cyclicalIndicator" Then
            alkaneBase.isCyclical = True
            counter = counter + 1
        End If

        For index = 0 To tokenDefinitions.Length - 1
            If tokenDefinitions(index).name = "multiplier" Then
                ' parse XML to get length from multiplier value
                Dim multipliers = tokenDefinitions(index).XML.Descendants("data").Descendants("multiplier").Descendants("prefix")
                For Each element In multipliers
                    If element.Value = mainChainTokens(counter).value Then
                        alkaneBase.length = CType(element.Parent.Attribute("number"), Integer)
                    End If
                Next
            End If
        Next

        ' error on cyclomethane or cycloethane
        If alkaneBase.length < 3 And alkaneBase.isCyclical Then
            errorPointer = ""
            ' add substituentOffset
            For offset = 0 To substituentOffset
                errorPointer = errorPointer & " "
            Next
            alkaneBase.isError = True
            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                & organicNameString & Environment.NewLine _
                & errorPointer & "^" & Environment.NewLine _
                & "cyclical main chain of length " & alkaneBase.length & " invalid"
            Return alkaneBase
        End If

        ' skip initial possible cyclical indicator and multiplier
        counter = counter + 1

        If counter > mainChainTokens.Length - 1 Then
            ' first token is after cyclicalIndicator if it is there is a multiplier
            errorPointer = ""
            ' add substituentOffset
            For offset = 0 To substituentOffset
                errorPointer = errorPointer & " "
            Next
            alkaneBase.isError = True
            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                & organicNameString & Environment.NewLine _
                & errorPointer & "^" & Environment.NewLine _
                & "malformed main chain - too short"
            Return alkaneBase
        End If

        ' used to check that the first token after the multiplier, other than hyphen + locantGroup + hyphen + [groupRepeater] is an alkAneEneYneSuffix
        Dim firstPass As Boolean
        firstPass = True

        ' next token can be alkane, if not the loop below will deal with it
        If mainChainTokens(counter).type = "alkAneEneYneSuffix" And (mainChainTokens(counter).value = "ane" Or mainChainTokens(counter).value = "an") Then
            ' skip over it, no information can be extracted from it (e.g. methane, the ane does not have any information).
            ' if the name is only a multiplier and a suffix, while loop will not activate (counter > amount of tokens)
            counter = counter + 1
            firstPass = False
        End If

        ' next set of tokens will be [hyphen + locantGroup + hyphen + [groupRepeater]] + aneEneYneSuffix
        Dim locantGroupIndex As Integer
        locantGroupIndex = -1
        While counter < mainChainTokens.Length
            If mainChainTokens(counter).type = "hyphen" Then
                ' skip to locantGroup
                counter = counter + 1
                If counter > mainChainTokens.Length - 1 Then
                    ' throw error if name ends here
                    errorPointer = ""
                    For tCounter = 0 To counter - 1
                        For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                            errorPointer = errorPointer & " "
                        Next
                    Next
                    ' add substituentOffset
                    For offset = 0 To substituentOffset
                        errorPointer = errorPointer & " "
                    Next
                    alkaneBase.isError = True
                    alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                        & organicNameString & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & "locantGroup expected"
                    Return alkaneBase
                End If
                If mainChainTokens(counter).type = "locantGroup" Then
                    locantGroupIndex = counter

                    ' stores tokens
                    locantGroupArray = mainChainTokens(locantGroupIndex).value.Split(",")

                    locantOffset = 0
                    ' check that locants are within lengths of main chain
                    For locantIndex = 0 To locantGroupArray.Length - 1
                        If CType(locantGroupArray(locantIndex), Integer) > alkaneBase.length Or CType(locantGroupArray(locantIndex), Integer) < 1 Then
                            errorPointer = ""
                            For tCounter = 0 To counter - 1
                                For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                    errorPointer = errorPointer & " "
                                Next
                            Next
                            ' add substituentOffset
                            For offset = 0 To substituentOffset
                                errorPointer = errorPointer & " "
                            Next
                            ' add locantOffset
                            locantOffset = locantOffset - 1
                            For offset = 0 To locantOffset
                                errorPointer = errorPointer & " "
                            Next
                            alkaneBase.isError = True
                            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                & organicNameString & Environment.NewLine _
                                & errorPointer & "^" & Environment.NewLine _
                                & "locant """ & locantGroupArray(locantIndex) & """ out of bounds"
                            Return alkaneBase
                        Else
                            ' length of locant name
                            For CIndex = 0 To locantGroupArray(locantIndex).Length - 1
                                locantOffset = locantOffset + 1
                            Next
                            ' comma
                            locantOffset = locantOffset + 1
                        End If
                    Next

                    ' OrElse is a short circuit or (doesn't evaluate the second condition if the first is true)
                    If counter + 1 > mainChainTokens.Length - 1 OrElse mainChainTokens(counter + 1).type <> "hyphen" Then
                        ' throw error (should be hyphen)
                        errorPointer = ""
                        For tCounter = 0 To counter
                            For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        ' add substituentOffset
                        For offset = 0 To substituentOffset
                            errorPointer = errorPointer & " "
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "hyphen expected"
                        Return alkaneBase
                    End If

                    ' skip after hyphen to next group
                    counter = counter + 2

                    If counter > mainChainTokens.Length - 1 Then
                        ' throw error (groupRepeater, alkAneEnYneSuffix or functionalGroupMainChainSuffix expected)
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        ' add substituentOffset
                        For offset = 0 To substituentOffset
                            errorPointer = errorPointer & " "
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "groupRepeater, alkAneEnYneSuffix or functionalGroupMainChainSuffix expected"
                        Return alkaneBase
                    End If

                    ' if token is a groupRepeater, skip again
                    If mainChainTokens(counter).type = "groupRepeater" Then
                        ' checking that groupRepeater matches with tokens
                        ' first get number of locants groupRepeater is implying in groupRepeaterNumber
                        For tokenDefIndex = 0 To tokenDefinitions.Length - 1
                            If tokenDefinitions(tokenDefIndex).name = "groupRepeater" Then
                                For Each groupRepeaterElement In tokenDefinitions(tokenDefIndex).XML.Descendants("data").Descendants("groupRepeater").Descendants("prefix")
                                    If groupRepeaterElement.Value = mainChainTokens(counter).value Then
                                        groupRepeaterNumber = groupRepeaterElement.Parent.Attribute("number")
                                    End If
                                Next
                            End If
                        Next
                        If locantGroupArray.Length <> groupRepeaterNumber Then
                            ' throw error (groupRepeater doesn't match number of locants)
                            errorPointer = ""
                            For tCounter = 0 To counter - 1
                                For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                    errorPointer = errorPointer & " "
                                Next
                            Next
                            ' add substituentOffset
                            For offset = 0 To substituentOffset
                                errorPointer = errorPointer & " "
                            Next
                            alkaneBase.isError = True
                            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                & organicNameString & Environment.NewLine _
                                & errorPointer & "^" & Environment.NewLine _
                                & "groupRepeater implies " & groupRepeaterNumber & " locant(s), but " & locantGroupArray.Length & " locant(s) present"
                            Return alkaneBase
                        End If
                        counter = counter + 1
                    Else
                        ' check if multiple locants but no groupRepeater given
                        If locantGroupArray.Length > 1 Then
                            errorPointer = ""
                            For tCounter = 0 To counter - 1
                                For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                    errorPointer = errorPointer & " "
                                Next
                            Next
                            ' add substituentOffset
                            For offset = 0 To substituentOffset
                                errorPointer = errorPointer & " "
                            Next
                            alkaneBase.isError = True
                            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                & organicNameString & Environment.NewLine _
                                & errorPointer & "^" & Environment.NewLine _
                                & locantGroupArray.Length & " locants present but no groupRepeater (di, tri, etc...) present"
                            Return alkaneBase
                        End If
                    End If
                    If mainChainTokens(counter).type = "alkAneEneYneSuffix" Then

                        ' must be an ene, en, yne or yn
                        Select Case mainChainTokens(counter).value
                            Case "ene", "en"
                                ' check that locants aren't out of bounds (special check for alkene/yne)
                                locantOffset = 0
                                ' check that locants are within lengths of main chain
                                For locantIndex = 0 To locantGroupArray.Length - 1
                                    If (CType(locantGroupArray(locantIndex), Integer) > alkaneBase.length - 1 Or CType(locantGroupArray(locantIndex), Integer) < 1) And Not (CType(locantGroupArray(locantIndex), Integer) = alkaneBase.length And alkaneBase.isCyclical) Then
                                        errorPointer = ""
                                        For tCounter = 0 To locantGroupIndex - 1
                                            For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                                errorPointer = errorPointer & " "
                                            Next
                                        Next
                                        ' add substituentOffset
                                        For offset = 0 To substituentOffset
                                            errorPointer = errorPointer & " "
                                        Next
                                        ' add locantOffset
                                        locantOffset = locantOffset - 1
                                        For offset = 0 To locantOffset
                                            errorPointer = errorPointer & " "
                                        Next
                                        alkaneBase.isError = True
                                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                            & organicNameString & Environment.NewLine _
                                            & errorPointer & "^" & Environment.NewLine _
                                            & "locant """ & locantGroupArray(locantIndex) & """ out of bounds"
                                        Return alkaneBase
                                    Else
                                        ' length of locant name
                                        For CIndex = 0 To locantGroupArray(locantIndex).Length - 1
                                            locantOffset = locantOffset + 1
                                        Next
                                        ' comma
                                        locantOffset = locantOffset + 1
                                    End If
                                Next

                                ' add alkene functional group to simpleSubstituent array
                                Dim alkenefunctionalGroup As ASTFunctionalGroup
                                alkenefunctionalGroup.type = "alkene"
                                alkenefunctionalGroup.locants = locantGroupArray

                                ReDim Preserve alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length)
                                alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length - 1) = alkenefunctionalGroup
                            Case "yne", "yn"
                                ' add alkene functional group to simpleSubstituent array
                                Dim alkynefunctionalGroup As ASTFunctionalGroup
                                alkynefunctionalGroup.type = "alkyne"
                                alkynefunctionalGroup.locants = locantGroupArray

                                ' check that locants aren't out of bounds (special check for alkene/yne)
                                locantOffset = 0
                                ' check that locants are within lengths of main chain
                                For locantIndex = 0 To locantGroupArray.Length - 1
                                    If (CType(locantGroupArray(locantIndex), Integer) > alkaneBase.length - 1 Or CType(locantGroupArray(locantIndex), Integer) < 1) And Not (CType(locantGroupArray(locantIndex), Integer) = alkaneBase.length And alkaneBase.isCyclical) Then
                                        errorPointer = ""
                                        For tCounter = 0 To locantGroupIndex - 1
                                            For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                                errorPointer = errorPointer & " "
                                            Next
                                        Next
                                        ' add substituentOffset
                                        For offset = 0 To substituentOffset
                                            errorPointer = errorPointer & " "
                                        Next
                                        ' add locantOffset
                                        locantOffset = locantOffset - 1
                                        For offset = 0 To locantOffset
                                            errorPointer = errorPointer & " "
                                        Next
                                        alkaneBase.isError = True
                                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                            & organicNameString & Environment.NewLine _
                                            & errorPointer & "^" & Environment.NewLine _
                                            & "locant """ & locantGroupArray(locantIndex) & """ out of bounds"
                                        Return alkaneBase
                                    Else
                                        ' length of locant name
                                        For CIndex = 0 To locantGroupArray(locantIndex).Length - 1
                                            locantOffset = locantOffset + 1
                                        Next
                                        ' comma
                                        locantOffset = locantOffset + 1
                                    End If
                                Next

                                ReDim Preserve alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length)
                                alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length - 1) = alkynefunctionalGroup
                            Case Else
                                ' throw error (can't have locant group and ane ending)
                                errorPointer = ""
                                For tCounter = 0 To counter - 1
                                    For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                        errorPointer = errorPointer & " "
                                    Next
                                Next
                                ' add substituentOffset
                                For offset = 0 To substituentOffset
                                    errorPointer = errorPointer & " "
                                Next
                                alkaneBase.isError = True
                                alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                    & organicNameString & Environment.NewLine _
                                    & errorPointer & "^" & Environment.NewLine _
                                    & "ene or yne suffix expected, not ane"
                                Return alkaneBase
                        End Select
                        ' move on to next token and set firstPass to false
                        counter = counter + 1
                        firstPass = False
                    Else
                        ' if not ane, ene or yne, must be a functionalGroupMainChainSuffix
                        If mainChainTokens(counter).type = "functionalGroupMainChainSuffix" Then
                            If firstPass Then
                                ' if this is the first pass, throw error as functionalGroupMainChainSuffix can't come before an alkAneEneYneSuffix
                                errorPointer = ""
                                For tCounter = 0 To counter - 1
                                    For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                        errorPointer = errorPointer & " "
                                    Next
                                Next
                                ' add substituentOffset
                                For offset = 0 To substituentOffset
                                    errorPointer = errorPointer & " "
                                Next
                                alkaneBase.isError = True
                                alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                    & organicNameString & Environment.NewLine _
                                    & errorPointer & "^" & Environment.NewLine _
                                    & "alkeAneEneYneSuffix expected before functionalGroupMainChainSuffix"
                                Return alkaneBase
                            End If
                            For index = 0 To tokenDefinitions.Length - 1
                                If tokenDefinitions(index).name = "functionalGroupMainChainSuffix" Then
                                    ' find matching suffix type, use its token definition XML to generate a functional group type for it
                                    ' then create a simple substituent and add to array
                                    Dim FGSubstituents = tokenDefinitions(index).XML.Descendants("data").Descendants("functionalGroupMainChainSuffix")
                                    For Each FGSub In FGSubstituents
                                        If FGSub.Attribute("identification") = mainChainTokens(counter).value Then
                                            Dim functionalGroupMainChainSuffixDefinition As String
                                            functionalGroupMainChainSuffixDefinition = FGSub.Element("type").Value

                                            ' throw error if ending mainChainSuffix not at end
                                            If {"carboxylic acid", "aldehyde"}.Contains(functionalGroupMainChainSuffixDefinition) And counter <> mainChainTokens.Length - 1 Then
                                                errorPointer = ""
                                                For tCounter = 0 To counter - 1
                                                    For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                                        errorPointer = errorPointer & " "
                                                    Next
                                                Next
                                                ' add substituentOffset
                                                For offset = 0 To substituentOffset
                                                    errorPointer = errorPointer & " "
                                                Next
                                                alkaneBase.isError = True
                                                alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                                    & organicNameString & Environment.NewLine _
                                                    & errorPointer & "^" & Environment.NewLine _
                                                    & functionalGroupMainChainSuffixDefinition & " suffix not positioned last"
                                                Return alkaneBase
                                            End If

                                            ' special case - a cyclical carboxylic acid is different from a normal one as it extends out with one carbon
                                            ' there is a special type for it - cyclical carboxylic acid
                                            If functionalGroupMainChainSuffixDefinition = "carboxylic acid" And alkaneBase.isCyclical Then
                                                functionalGroupMainChainSuffixDefinition = "cyclical carboxylic acid"
                                            End If

                                            Dim functionalGroupMainChainSuffixGroup As ASTFunctionalGroup
                                            functionalGroupMainChainSuffixGroup.type = functionalGroupMainChainSuffixDefinition
                                            functionalGroupMainChainSuffixGroup.locants = locantGroupArray

                                            ReDim Preserve alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length)
                                            alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length - 1) = functionalGroupMainChainSuffixGroup
                                        End If
                                    Next
                                End If
                            Next
                            ' move on to next token
                            counter = counter + 1
                        Else
                            ' throw error must be functionalGroupMainChainSuffix or alkAneEneYneSuffix expected
                            errorPointer = ""
                            For tCounter = 0 To counter - 1
                                For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                                    errorPointer = errorPointer & " "
                                Next
                            Next
                            ' add substituentOffset
                            For offset = 0 To substituentOffset
                                errorPointer = errorPointer & " "
                            Next
                            alkaneBase.isError = True
                            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                & organicNameString & Environment.NewLine _
                                & errorPointer & "^" & Environment.NewLine _
                                & "functionalGroupMainChainSuffix or alkAneEneYneSuffix expected"
                            Return alkaneBase
                        End If
                    End If
                Else
                    ' throw error (in this case a locantGroup must follow the hyphen)
                    errorPointer = ""
                    For tCounter = 0 To counter - 1
                        For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                            errorPointer = errorPointer & " "
                        Next
                    Next
                    ' add substituentOffset
                    For offset = 0 To substituentOffset
                        errorPointer = errorPointer & " "
                    Next
                    alkaneBase.isError = True
                    alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                        & organicNameString & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & "locantGroup expected"
                    Return alkaneBase
                End If
            Else
                ' throw error (this token must be a hyphen)
                errorPointer = ""
                For tCounter = 0 To counter - 1
                    For tokenChars = 0 To mainChainTokens(tCounter).value.Length - 1
                        errorPointer = errorPointer & " "
                    Next
                Next
                ' add substituentOffset
                For offset = 0 To substituentOffset
                    errorPointer = errorPointer & " "
                Next
                alkaneBase.isError = True
                alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                    & organicNameString & Environment.NewLine _
                    & errorPointer & "^" & Environment.NewLine _
                    & "hyphen expected (locants must always be specified)"
                Return alkaneBase
            End If
        End While


        ' 3. parse substituents
        '   case 1. locantGroup + hyphen + [groupRepeater] + functionalGroupSubstituent
        '   case 2. locantGroup + hyphen + [groupRepeater] + <complex bracketed substituent, needs to be solved recursively>
        '   case 3. locantGroup + hyphen + [groupRepeater] + <complex unbracketed substituent, needs to be solved recursively>
        ' case 1 and 2 are easy to see and pick up, case 3 an be detected as after the hyphen or grouprepeater there will be a multiplier, the end of the unbracketed complex substituent will be a substituentEnd token
        ' complex unbracketed substituents always are just like the main chain of an alkene and will always end with yl

        Dim complexSubstituentTokens As Token()
        complexSubstituentTokens = {}
        Dim bracketCount As Integer
        Dim bracket As String
        Dim oppositeBracket As String

        Dim newComplexSubstituent As ASTAlkaneBase
        Dim newComplexSubstituentString As String

        Dim moveCounter As Boolean
        moveCounter = True

        ' reset counter and locantGroupIndex
        locantGroupIndex = -1
        counter = -1
        While counter < substituentTokens.Length - 1
            If moveCounter Then
                moveCounter = False
                counter = counter + 1
            End If
            If substituentTokens(counter).type = "hyphen" Then
                ' move on if hyphen, but ignore if end of substituent array (there can be a hyphen between the main chain and substituents)
                counter = counter + 1
                If counter < substituentTokens.Length AndAlso substituentTokens(counter).type <> "locantGroup" Then
                    errorPointer = ""
                    For tCounter = 0 To counter - 1
                        For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                            errorPointer = errorPointer & " "
                        Next
                    Next
                    alkaneBase.isError = True
                    alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                        & organicNameString & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & "locantGroup expected"
                    Return alkaneBase
                End If
            Else
                If substituentTokens(counter).type <> "locantGroup" Then
                    If (counter - 1 < substituentTokens.Length - 1 And counter - 1 > 0) AndAlso substituentTokens(counter - 1).type <> "hyphen" Then
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "hyphen expected"
                        Return alkaneBase
                    Else
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "locantGroup expected"
                        Return alkaneBase
                    End If
                End If
            End If
            If counter < substituentTokens.Length - 1 AndAlso substituentTokens(counter).type = "locantGroup" Then
                locantGroupIndex = counter

                ' stores tokens
                locantGroupArray = substituentTokens(locantGroupIndex).value.Split(",")

                locantOffset = 0
                ' check that locants are within lengths of main chain
                For locantIndex = 0 To locantGroupArray.Length - 1
                    If CType(locantGroupArray(locantIndex), Integer) > alkaneBase.length Or CType(locantGroupArray(locantIndex), Integer) < 1 Then
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        ' add locantOffset
                        locantOffset = locantOffset - 1
                        For offset = 0 To locantOffset
                            errorPointer = errorPointer & " "
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "locant """ & locantGroupArray(locantIndex) & """ out of bounds"
                        Return alkaneBase
                    Else
                        ' length of locant name
                        For CIndex = 0 To locantGroupArray(locantIndex).Length - 1
                            locantOffset = locantOffset + 1
                        Next
                        ' comma
                        locantOffset = locantOffset + 1
                    End If
                Next

                ' if next group not hyphen throw error
                If substituentTokens(counter + 1).type <> "hyphen" Then
                    errorPointer = ""
                    For tCounter = 0 To counter
                        For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                            errorPointer = errorPointer & " "
                        Next
                    Next
                    alkaneBase.isError = True
                    alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                        & organicNameString & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & "hyphen expected"
                    Return alkaneBase
                End If

                ' skip to to after hyphen
                counter = counter + 2
                If counter > substituentTokens.Length - 1 Then
                    ' throw error if nothing after hyphen
                    errorPointer = ""
                    For tCounter = 0 To counter - 1
                        For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                            errorPointer = errorPointer & " "
                        Next
                    Next
                    alkaneBase.isError = True
                    alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                        & organicNameString & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & "start of substituent expected"
                    Return alkaneBase
                End If

                ' if token is a groupRepeater, skip again
                If substituentTokens(counter).type = "groupRepeater" Then
                    ' checking that groupRepeater matches with tokens
                    ' first get number of locants groupRepeater is implying in groupRepeaterNumber
                    For tokenDefIndex = 0 To tokenDefinitions.Length - 1
                        If tokenDefinitions(tokenDefIndex).name = "groupRepeater" Then
                            For Each groupRepeaterElement In tokenDefinitions(tokenDefIndex).XML.Descendants("data").Descendants("groupRepeater").Descendants("prefix")
                                If groupRepeaterElement.Value = substituentTokens(counter).value Then
                                    groupRepeaterNumber = groupRepeaterElement.Parent.Attribute("number")
                                End If
                            Next
                        End If
                    Next
                    If locantGroupArray.Length <> groupRepeaterNumber Then
                        ' throw error (groupRepeater doesn't match number of locants)
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "groupRepeater implies " & groupRepeaterNumber & " locant(s), but " & locantGroupArray.Length & " locant(s) present"
                        Return alkaneBase
                    End If
                    counter = counter + 1
                Else
                    ' check if multiple locants but no groupRepeater given
                    If locantGroupArray.Length > 1 Then
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & locantGroupArray.Length & " locants present but no groupRepeater (di, tri, etc...) present"
                        Return alkaneBase
                    End If
                End If
                If substituentTokens(counter).type = "functionalGroupSubstituent" Then
                    ' possible simple substituent
                    For index = 0 To tokenDefinitions.Length - 1
                        If tokenDefinitions(index).name = "functionalGroupSubstituent" Then
                            ' find matching substituent type, use its token definition XML to generate a functional group type for it
                            ' then create a simple substituent and add to array
                            Dim FGSubstituents = tokenDefinitions(index).XML.Descendants("data").Descendants("functionalGroupSubstituent")
                            For Each FGSub In FGSubstituents
                                If FGSub.Attribute("identification") = substituentTokens(counter).value Then
                                    Dim functionalGroupMainSubstituentDefinition As String
                                    functionalGroupMainSubstituentDefinition = FGSub.Element("type").Value

                                    ' special case - a cyclical carboxylic acid is different from a normal one as it extends out with one carbon
                                    ' there is a special type for it - cyclical carboxylic acid
                                    If functionalGroupMainSubstituentDefinition = "carboxylic acid" And alkaneBase.isCyclical Then
                                        functionalGroupMainSubstituentDefinition = "cyclical carboxylic acid"
                                    End If

                                    Dim functionalGroupMainSubstituentGroup As ASTFunctionalGroup
                                    functionalGroupMainSubstituentGroup.type = functionalGroupMainSubstituentDefinition
                                    functionalGroupMainSubstituentGroup.locants = substituentTokens(locantGroupIndex).value.Split(",")

                                    ReDim Preserve alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length)
                                    alkaneBase.simpleSubstituents(alkaneBase.simpleSubstituents.Length - 1) = functionalGroupMainSubstituentGroup
                                End If
                            Next
                        End If
                    Next
                    ' move on to next token on next loop
                    moveCounter = True
                Else
                    If substituentTokens(counter).type = "openBracket" Then
                        complexSubstituentTokens = {}
                        ' possible complex bracketed substituent
                        bracketCount = 1
                        bracket = substituentTokens(counter).value
                        Select Case bracket
                            Case "("
                                oppositeBracket = ")"
                            Case "{"
                                oppositeBracket = "}"
                            Case Else
                                oppositeBracket = "]"
                        End Select
                        Dim startBracketIndex As Integer
                        startBracketIndex = counter
                        ' check for matching bracket
                        While bracketCount <> 0 And counter < substituentTokens.Length - 1
                            counter = counter + 1
                            ReDim Preserve complexSubstituentTokens(complexSubstituentTokens.Length)
                            complexSubstituentTokens(complexSubstituentTokens.Length - 1) = substituentTokens(counter)
                            Select Case substituentTokens(counter).value
                                Case bracket
                                    bracketCount = bracketCount + 1
                                Case oppositeBracket
                                    bracketCount = bracketCount - 1
                            End Select
                        End While

                        ' remove last closeBracket added in while loop
                        ' if length is 0 there is a problem (an error will be thrown because no matching closed bracket would have been found)
                        If complexSubstituentTokens.Length <> 0 Then
                            Array.Resize(complexSubstituentTokens, complexSubstituentTokens.Length - 1)
                        End If

                        ' creating string for recursion
                        newComplexSubstituentString = ""
                        For CSToken = 0 To complexSubstituentTokens.Length - 1
                            newComplexSubstituentString = newComplexSubstituentString & complexSubstituentTokens(CSToken).value
                        Next

                        If bracketCount = 0 Then
                            ' means matching bracket was found and complexSubstituentTokens was filled in completely
                            ' remove last substituentEnd group
                            If complexSubstituentTokens(complexSubstituentTokens.Length - 1).type = "substituentEnd" Then
                                Array.Resize(complexSubstituentTokens, complexSubstituentTokens.Length - 1)
                            End If
                            If complexSubstituentTokens(complexSubstituentTokens.Length - 1).type = "multiplier" Then
                                ' converting entries like in the case of (methyl) -> meth -> methane
                                ReDim Preserve complexSubstituentTokens(complexSubstituentTokens.Length)
                                Dim aneToken As Token
                                aneToken.type = "alkAneEneYneSuffix"
                                aneToken.value = "ane"
                                complexSubstituentTokens(complexSubstituentTokens.Length - 1) = aneToken
                            End If


                            ' recursing to generate an ASTAlkaneBase structure to add to the complexSubstituents array
                            ' return immediately if error
                            newComplexSubstituent = generateASTAlkaneBase(newComplexSubstituentString, complexSubstituentTokens, False, substituentTokens(locantGroupIndex).value.Split(","), tokenDefinitions, functionalGroupDefinitions)
                            If newComplexSubstituent.isError Then
                                Return newComplexSubstituent
                            Else
                                ReDim Preserve alkaneBase.complexSubstituents(alkaneBase.complexSubstituents.Length)
                                alkaneBase.complexSubstituents(alkaneBase.complexSubstituents.Length - 1) = newComplexSubstituent
                            End If
                            moveCounter = True
                        Else
                            ' throw error (no matching bracket found)
                            errorPointer = ""
                            For tCounter = 0 To startBracketIndex - 1
                                For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                    errorPointer = errorPointer & " "
                                Next
                            Next
                            alkaneBase.isError = True
                            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                & organicNameString & Environment.NewLine _
                                & errorPointer & "^" & Environment.NewLine _
                                & "no matching bracket found"
                            Return alkaneBase
                        End If
                    Else
                        If substituentTokens(counter).type = "cyclicalIndicator" Or substituentTokens(counter).type = "multiplier" Then
                            complexSubstituentTokens = {}
                            ' possible complex unbracketed substituent
                            Dim startCounter As Integer
                            startCounter = counter
                            While counter < substituentTokens.Length - 1 AndAlso substituentTokens(counter).type <> "substituentEnd"
                                ReDim Preserve complexSubstituentTokens(complexSubstituentTokens.Length)
                                complexSubstituentTokens(complexSubstituentTokens.Length - 1) = substituentTokens(counter)
                                counter = counter + 1
                            End While

                            If substituentTokens(counter).type <> "substituentEnd" Then
                                ' this means there is no substituent end, i.e. there is no yl
                                ' throw error
                                errorPointer = ""
                                For tCounter = 0 To startCounter - 1
                                    For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                        errorPointer = errorPointer & " "
                                    Next
                                Next
                                alkaneBase.isError = True
                                alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                    & organicNameString & Environment.NewLine _
                                    & errorPointer & "^" & Environment.NewLine _
                                    & "no substituentEnd ('yl') found for this substituent"
                                Return alkaneBase
                            End If

                            ' the ending substituentEnd is not in the complexSubstituentTokens array
                            ' instead replacing it with an token 'ane token'
                            ' e.g. methyl -> meth -> methane
                            ' also make string for recursion beforehand

                            newComplexSubstituentString = ""
                            For CSToken = 0 To complexSubstituentTokens.Length - 1
                                newComplexSubstituentString = newComplexSubstituentString & complexSubstituentTokens(CSToken).value
                            Next
                            newComplexSubstituentString = newComplexSubstituentString & "yl"

                            If complexSubstituentTokens(complexSubstituentTokens.Length - 1).type <> "alkAneEneYneSuffix" Then
                                ReDim Preserve complexSubstituentTokens(complexSubstituentTokens.Length)
                                Dim aneToken As Token
                                aneToken.type = "alkAneEneYneSuffix"
                                aneToken.value = "ane"
                                complexSubstituentTokens(complexSubstituentTokens.Length - 1) = aneToken
                            End If

                            ' recursing to generate an ASTAlkaneBase structure to add to the complexSubstituents array
                            newComplexSubstituent = generateASTAlkaneBase(newComplexSubstituentString, complexSubstituentTokens, False, substituentTokens(locantGroupIndex).value.Split(","), tokenDefinitions, functionalGroupDefinitions)

                            If newComplexSubstituent.isError Then
                                Return newComplexSubstituent
                            Else
                                ReDim Preserve alkaneBase.complexSubstituents(alkaneBase.complexSubstituents.Length)
                                alkaneBase.complexSubstituents(alkaneBase.complexSubstituents.Length - 1) = newComplexSubstituent
                            End If
                            moveCounter = True
                        Else
                            ' throw error (substituent start expected)
                            errorPointer = ""
                            For tCounter = 0 To counter - 1
                                For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                    errorPointer = errorPointer & " "
                                Next
                            Next
                            alkaneBase.isError = True
                            alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                                & organicNameString & Environment.NewLine _
                                & errorPointer & "^" & Environment.NewLine _
                                & "start of substituent expected"
                            Return alkaneBase
                        End If
                    End If
                End If
            Else
                ' if last item is a locantgroup - error
                If counter < substituentTokens.Length Then
                    If substituentTokens(counter).type = "locantGroup" And counter = substituentTokens.Length - 1 Then
                        errorPointer = ""
                        For tCounter = 0 To counter - 1
                            For tokenChars = 0 To substituentTokens(tCounter).value.Length - 1
                                errorPointer = errorPointer & " "
                            Next
                        Next
                        alkaneBase.isError = True
                        alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                            & organicNameString & Environment.NewLine _
                            & errorPointer & "^" & Environment.NewLine _
                            & "invalid position for locantGroup"
                        Return alkaneBase
                    End If
                End If

            End If

        End While

        ' --- check bonds in carbons to see if compound possible ---
        ' if not, throw error
        ' set up bonds in carbon chain
        Dim availableBonds As Integer()
        ReDim availableBonds(alkaneBase.length - 1)
        For bondIndex = 0 To availableBonds.Length - 1
            availableBonds(bondIndex) = 2
        Next
        If Not alkaneBase.isCyclical Then
            availableBonds(0) = 3
            availableBonds(availableBonds.Length - 1) = 3
        End If
        ' exception for methane
        If alkaneBase.length = 1 Then
            availableBonds(0) = 4
        End If

        ' functional groups
        Dim transformedLocantIndex As Integer
        For simpleSubstituentIndex = 0 To alkaneBase.simpleSubstituents.Length - 1
            For locantIndex = 0 To alkaneBase.simpleSubstituents(simpleSubstituentIndex).locants.Length - 1
                transformedLocantIndex = CType(alkaneBase.simpleSubstituents(simpleSubstituentIndex).locants(locantIndex), Integer) - 1

                Select Case alkaneBase.simpleSubstituents(simpleSubstituentIndex).type
                    ' for ene and yne, deduction is for current locant and next one too (as it is a carbon-carbon bond)
                    Case "alkene"
                        availableBonds(transformedLocantIndex) = availableBonds(transformedLocantIndex) - 1
                        availableBonds(transformedLocantIndex + 1) = availableBonds(transformedLocantIndex + 1) - 1
                    Case "alkyne"
                        availableBonds(transformedLocantIndex) = availableBonds(transformedLocantIndex) - 2
                        availableBonds(transformedLocantIndex + 1) = availableBonds(transformedLocantIndex + 1) - 2
                    Case Else
                        ' find appropriate functional group and deduct according to first level children
                        For functionalGroupIndex = 0 To functionalGroupDefinitions.Length - 1
                            If functionalGroupDefinitions(functionalGroupIndex).type = alkaneBase.simpleSubstituents(simpleSubstituentIndex).type Then
                                For childIndex = 0 To functionalGroupDefinitions(functionalGroupIndex).children.Length - 1
                                    availableBonds(transformedLocantIndex) = availableBonds(transformedLocantIndex) - functionalGroupDefinitions(functionalGroupIndex).children(childIndex).bondType
                                Next
                            End If
                        Next
                End Select
            Next
        Next

        ' complex substituents
        For complexSubstituentIndex = 0 To alkaneBase.complexSubstituents.Length - 1
            For locantIndex = 0 To alkaneBase.complexSubstituents(complexSubstituentIndex).locants.Length - 1
                transformedLocantIndex = CType(alkaneBase.complexSubstituents(complexSubstituentIndex).locants(locantIndex), Integer) - 1
                availableBonds(transformedLocantIndex) = availableBonds(transformedLocantIndex) - 1
            Next
        Next

        ' if any available bonds negative
        For bondIndex = 0 To availableBonds.Length - 1
            If availableBonds(bondIndex) < 0 Then
                alkaneBase.isError = True
                alkaneBase.errorMessage = "the name """ & organicNameString & """ is not valid" & Environment.NewLine _
                    & organicNameString & Environment.NewLine _
                    & "^" & Environment.NewLine _
                    & "carbon at locant " & (bondIndex + 1) & " overloaded with " & (4 + availableBonds(bondIndex) * -1) & " bonds (carbon valency: 4)"
                Return alkaneBase
            End If
        Next

        Return alkaneBase

    End Function

    ''' <summary>
    ''' Generates an Abstract Syntax Tree in the format of an ASTRoot structure for a given organicCompound
    ''' </summary>
    ''' <param name="organicNameString">A string containing the name of the compound to be parsed</param>
    ''' <param name="organicNameTokens">
    ''' An array of Token structures representing the compound to be parsed. Can be generated with the generateTokens 
    ''' function in this module.
    ''' </param>
    ''' <param name="tokenDefinitions">
    ''' An array of TokenDefinition structures representing the definitions of tokens present in the organicNameTokens array.
    ''' Can be generated with the generateTokenDefinitions function in this module.
    ''' </param>
    ''' <returns>
    ''' Returns an ASTRoot structure representing the organicCompound. If the name is invalid, compoundTree.isError will be 
    ''' true and compoundTree.errorMessage will contain the error message.
    ''' </returns>
    ''' <remarks>An ASTRoot structure representing the parsed organicCompound</remarks>
    Public Function generateAST(ByVal organicNameString As String, ByVal organicNameTokens As Token(), ByVal tokenDefinitions As TokenDefinition(), ByVal functionalGroupDefinitions As FunctionalGroupDefinition()) As ASTRoot
        ' create root
        Dim ast As ASTRoot
        ast.organicName = organicNameString
        ' call generation function 
        ast.compoundTree = generateASTAlkaneBase(organicNameString, organicNameTokens, True, {}, tokenDefinitions, functionalGroupDefinitions)
        Return ast
    End Function

    ''' <summary>
    ''' Generates a FunctionalGroupChild from a XElement the functional group is stored in. Can handle recursive cases.
    ''' Doesn't initialise branches.
    ''' </summary>
    ''' <param name="childXML">The XElement the child is stored in</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Function generateFunctionalGroupChild(ByVal childXML As XElement) As FunctionalGroupChild
        Dim newFunctionalGroupChild As FunctionalGroupChild

        ' base case and recursive case
        newFunctionalGroupChild.bondType = CType(childXML.Element("bondType").Value, Integer)
        newFunctionalGroupChild.representor = childXML.Element("representor").Value
        newFunctionalGroupChild.colour = childXML.Element("colour").Value
        newFunctionalGroupChild.children = {}
        Dim branches As ConnectingBranches
        branches.takenBranches = {}
        newFunctionalGroupChild.branches = branches

        ' recursive case (base case - no children)
        For Each child In childXML.Elements("child")
            ReDim Preserve newFunctionalGroupChild.children(newFunctionalGroupChild.children.Length)
            newFunctionalGroupChild.children(newFunctionalGroupChild.children.Length - 1) = generateFunctionalGroupChild(child)
        Next

        Return newFunctionalGroupChild
    End Function

    ''' <summary>
    ''' Given the XElement of the XMl file containing the functional group definitions, parses them into a list of FunctionalGroupDefinition
    ''' structures. Can handle recursive cases.
    ''' </summary>
    ''' <param name="allFunctionalGroupDefinitions">XElement containing functional group definitions file</param>
    ''' <returns>An array of FunctionalGroupDefinitions containing the definitions of the </returns>
    ''' <remarks></remarks>
    Public Function generateFunctionalGroupDefinitions(ByVal allFunctionalGroupDefinitions As XElement) As FunctionalGroupDefinition()
        Dim definitions As FunctionalGroupDefinition()
        definitions = {}
        For Each functionalGroupDefinition In allFunctionalGroupDefinitions.Elements()
            Dim newDefinition As FunctionalGroupDefinition
            newDefinition.type = functionalGroupDefinition.Attribute("type")
            newDefinition.XML = functionalGroupDefinition
            newDefinition.children = {}

            ' cycle through children and generate FunctionalGroupChild representing them
            For Each child In functionalGroupDefinition.Elements("child")
                ReDim Preserve newDefinition.children(newDefinition.children.Length)
                newDefinition.children(newDefinition.children.Length - 1) = generateFunctionalGroupChild(child)
            Next

            ' add to return list
            ReDim Preserve definitions(definitions.Length)
            definitions(definitions.Length - 1) = newDefinition
        Next

        Return definitions
    End Function

    ''' <summary>
    ''' Given a singular token definition as an XML node and all the token definitions in XML format, return a TokenDefinition
    ''' structure representing the token definition. Handles unpacking regexes and expanding contained tokens within other tokens
    ''' recursively.
    ''' For more details regarding the parsing of XML, see the 'tokens.xml' file in the resources folder in the startup directory.
    ''' </summary>
    ''' <param name="tokenDefinitionXML">An XElement containing the token definition to be converted</param>
    ''' <param name="allTokenDefinitionsXML">An XElement containing all token definitions (used when tokenDefinitionXML contains other tokens)</param>
    ''' <returns>Returns a TokenDefinition representing tokenDefinitionXML</returns>
    ''' <remarks>Works for recursive definitions and expanded regexes.</remarks>
    Private Function generateTokenDefinition(ByVal tokenDefinitionXML As XElement, ByVal allTokenDefinitionsXML As XElement) As TokenDefinition
        ' created token definition
        Dim tokenDefinition As TokenDefinition

        ' add name, isBooleanRegex, isSpecial and XML to definition (same for base case and recursive case)
        tokenDefinition.name = tokenDefinitionXML.Attribute("name").Value
        tokenDefinition.XML = tokenDefinitionXML
        If tokenDefinitionXML.Element("type").Value = "boolean" Then
            tokenDefinition.isBooleanRegex = True
        Else
            tokenDefinition.isBooleanRegex = False
        End If
        If tokenDefinitionXML.Element("special").Value = "true" Then
            tokenDefinition.isSpecial = True
        Else
            tokenDefinition.isSpecial = False
        End If

        Dim unpack As XElement
        unpack = tokenDefinitionXML.Element("unpack")

        ' check if token definition is composed of other token definitions
        If IsNothing(unpack) Then
            ' the token definition is not composed of other token definitions (base case)
            ' so there are no contained elements
            tokenDefinition.contained = {}

            ' add token regex to definition
            Dim regex As String
            regex = tokenDefinitionXML.Element("regex").Value

            ' expand regex if needed
            If regex.Contains("%%") Then
                ' find part of regex to replace
                Dim expandingPartToReplaceRegex As New Regex("%%(.*)%%")
                Dim expandingPartToReplace As String
                expandingPartToReplace = expandingPartToReplaceRegex.Match(regex).Groups(1).Value

                ' seperate expanding part into components
                Dim expandingPart As String()
                expandingPart = expandingPartToReplace.Split(">")

                ' follow specified query of nodes in expandingPart to find requested elements
                ' e.g. if expandingPartToReplace = data>multiplier>prefix, collect all prefix nodes that have a multiplier parent
                ' that themselves have a data parent.
                ' data>multiplier>prefix#attr would mean collect nodes as above but the prefix node has to have an "attr"
                ' attribute.
                Dim currentElementXDoc = New XDocument(tokenDefinitionXML)
                Dim expandingPartQuery = currentElementXDoc.Root.Descendants(expandingPart(0))
                For index = 1 To expandingPart.Length - 1
                    expandingPartQuery = expandingPartQuery.Descendants(expandingPart(index).Split("#")(0))
                Next

                ' generate expanded regex
                Dim generatedRegex As String
                generatedRegex = ""

                Dim tempRegex As String
                tempRegex = ""
                For Each element In expandingPartQuery
                    If expandingPart(expandingPart.Length - 1).Contains("#") Then
                        tempRegex = element.Attribute(expandingPart(expandingPart.Length - 1).Split("#")(1)).Value
                    Else
                        tempRegex = element.Value
                    End If
                    generatedRegex = generatedRegex & "|" & System.Text.RegularExpressions.Regex.Replace(regex, "%%.*%%", tempRegex)
                Next

                ' remove extra "|" at start of regex
                generatedRegex = generatedRegex.Remove(0, 1)

                regex = generatedRegex
            End If

            tokenDefinition.regex = regex

            ' create array of followers
            Dim followers As String()
            ReDim followers(tokenDefinitionXML.Element("followers").Elements().Count - 1)
            Dim counter As Integer
            counter = 0
            For Each followerToken In tokenDefinitionXML.Element("followers").Elements()
                followers(counter) = followerToken.Value
                counter = counter + 1
            Next

            ' add token followers into definition
            tokenDefinition.followers = followers
        Else
            ' the token definition is composed of other token definitions (recursive case)

            Dim contained As String()
            contained = {}

            ' create array of contained tokens' XElements
            Dim containedTokens As XElement()
            ReDim containedTokens(tokenDefinitionXML.Element("unpack").Elements().Count - 1)
            Dim counter As Integer
            counter = 0
            For Each containedToken In tokenDefinitionXML.Element("unpack").Elements()
                ' find XElement of contained token and add to array
                For Each containedTokenDefinition In allTokenDefinitionsXML.Elements()
                    If containedTokenDefinition.Attribute("name").Value = containedToken.Value Then
                        containedTokens(counter) = containedTokenDefinition

                        ReDim Preserve contained(contained.Length)
                        contained(contained.Length - 1) = containedToken.Value
                    End If
                Next
                counter = counter + 1
            Next

            ' chain together regexes and followers of all contained tokens
            Dim regex As String
            regex = ""
            Dim followers As String()
            followers = {}
            Dim tokenDefContained As TokenDefinition
            For index = 0 To containedTokens.Length - 1
                tokenDefContained = generateTokenDefinition(containedTokens(index), allTokenDefinitionsXML)
                ' the "|" operater is an alternator (or), so below is saying - the current regex OR the regex of this token
                regex = regex & "|" & tokenDefContained.regex
                ' adding the current followers array and the followers array of the token together
                followers = followers.Concat(tokenDefContained.followers).ToArray()
                ' adding the current contained array and the contained array of the token together
                contained = contained.Concat(tokenDefContained.contained).ToArray()
            Next

            ' removing extra "|" at start of regex and duplicate items in followers and contained
            regex = regex.Remove(0, 1)
            followers = followers.Distinct().ToArray()
            contained = contained.Distinct().ToArray()

            ' putting pieces together to form new tokenDefinition
            tokenDefinition.regex = regex
            tokenDefinition.followers = followers
            tokenDefinition.contained = contained
        End If

        Return tokenDefinition
    End Function

    ''' <summary>
    ''' Given a XElement containing all token definitions in XML format, return an array of TokenDefinitions detailing given tokens.
    ''' For more details regarding the parsing of XML, see the 'tokens.xml' file in the resources folder in the startup directory.
    ''' </summary>
    ''' <param name="allTokenDefinitionsXML">An XElement containing all tokens to be converted into a TokenDefinitions</param>
    ''' <returns>An array of TokenDefinitions</returns>
    ''' <remarks>Uses the createTokenDefinition function to create singular TokenDefinitions</remarks>
    Public Function generateTokenDefinitions(ByVal allTokenDefinitionsXML As XElement) As TokenDefinition()
        ' created token definitions array
        Dim tokenDefinitions As TokenDefinition()
        ReDim tokenDefinitions(allTokenDefinitionsXML.Elements().Count - 1)

        ' create individual tokenDefinitions and add to array
        Dim counter As Integer
        counter = 0
        For Each tokenDefinitionXML In allTokenDefinitionsXML.Elements()
            tokenDefinitions(counter) = generateTokenDefinition(tokenDefinitionXML, allTokenDefinitionsXML)
            counter = counter + 1
        Next

        Return tokenDefinitions
    End Function

    ''' <summary>
    ''' Given an organicName and a tokenDefinition list, tokenise it, meaning convert the organic name into an array of Token structures.
    ''' The token array is then verified. If any error is encountered, returns the error in the form of an array of a single Token where
    ''' type="ERROR" and value contains the error details.
    ''' For more details regarding the parsing of XML, see the 'tokens.xml' file in the resources folder in the startup directory.
    ''' </summary>
    ''' <param name="organicName">A string containing the organicName to be tokenised</param>
    ''' <param name="tokenDefinitions">An array of tokenDefinitions used to tokenise the name</param>
    ''' <returns>An array of Token structures representing the organic name OR an array containing a single token whose type is ERROR and value contains an error message.</returns>
    ''' <remarks>The createTokenDefinitions function can be used to generate tokenDefinitions</remarks>
    Public Function generateTokens(ByVal organicName As String, ByVal tokenDefinitions As TokenDefinition()) As Token()
        ' step 1. create a token list

        ' keeps track of current position in name
        Dim index As Integer
        index = -1

        ' array to store tokens
        Dim tokens As Token()
        tokens = {}

        ' current character
        Dim currentChar As Char

        ' stores a built up string that is checked against all token definitions that when matched, gets turned into a token
        Dim possibleToken As String
        possibleToken = ""

        ' when possibleToken is verified to be a token, lastTokenHit stores the token
        Dim lastTokenHit As Token
        ' flag values
        lastTokenHit.type = ""
        lastTokenHit.value = "-1"

        ' keeps track of index of last token hit
        Dim indexAfterLastTokenHit As Integer
        indexAfterLastTokenHit = -1

        ' stores the current token definition that possibleToken is being checked against
        Dim currentTokenDef As TokenDefinition

        ' special case - error empty name
        If organicName = "" Then
            Dim errorToken As Token
            errorToken.type = "ERROR"
            errorToken.value = "the name """ & organicName & """ is not valid" & Environment.NewLine _
                & organicName & Environment.NewLine _
                & "^" & Environment.NewLine _
                & """"" is not valid (no matching tokens)"
            Return {errorToken}
        End If

        Dim regexTest As Regex
        Dim match As Match

        While index < organicName.Length - 1
            index = index + 1
            currentChar = organicName(index)
            possibleToken = possibleToken & currentChar

            ' loop through all tokens and try to see if possibleToken has any matches
            For tokenIndex = 0 To tokenDefinitions.Length - 1
                currentTokenDef = tokenDefinitions(tokenIndex)

                If currentTokenDef.isBooleanRegex Then
                    ' testing for boolean regex (yes or no, no matching groups)
                    regexTest = New Regex(currentTokenDef.regex)

                    If regexTest.IsMatch(possibleToken) Then
                        ' if a match and token not special, record it in lastHitToken (boolean regexes have empty values)
                        If Not currentTokenDef.isSpecial Then
                            lastTokenHit.type = currentTokenDef.name
                            lastTokenHit.value = ""
                            indexAfterLastTokenHit = index
                        End If
                    End If
                Else
                    ' testing for multiway regex (matching groups)
                    regexTest = New Regex(currentTokenDef.regex)
                    match = regexTest.Match(possibleToken)

                    ' match length 0 means no match
                    If match.Length <> 0 Then
                        ' if a match and token not special record it in lastHitToken (multiway regexes have the match as the value)
                        If Not currentTokenDef.isSpecial Then
                            lastTokenHit.type = currentTokenDef.name
                            lastTokenHit.value = match.Value
                            indexAfterLastTokenHit = index
                            ' avoiding clash between groupRepeater and multiplier tokens (otherwise pentane -> penta + ne -> ERROR)
                            If currentTokenDef.name = "multiplier" Then
                                ' if current token is a multiplier and the next 3 characters are "ane", "an-" or "any", skip index to end of name
                                ' e.g. in pentane, pentan-1-oic acid, pentanyl, pent is a multiplier
                                ' e.g. but in pentanitro, penta is a groupRepeater
                                ' otherwise tokeniser will tokenise it as a groupRepeater. E.g. pentane -> pent (multiplier) -> penta (groupRepeater) -> ERROR - unkown token "ne"
                                If index + 3 < organicName.Length AndAlso (organicName(index + 1) = "a" And organicName(index + 2) = "n" And (organicName(index + 3) = "e" Or organicName(index + 3) = "-" Or organicName(index + 3) = "y")) Then
                                    ' skipping to end 
                                    index = organicName.Length - 1
                                End If
                            End If
                        End If
                    End If
                End If
            Next

            If index = organicName.Length - 1 Then
                ' search has gone to end of name

                If lastTokenHit.value = "-1" Then
                    ' if no lastHitToken, current name section is not valid

                    ' create error message and return early
                    Dim errorMessage As String
                    Dim errorPointer As String
                    errorPointer = ""
                    For tokenIndex = 0 To tokens.Length - 1
                        For tokenStringIndex = 0 To tokens(tokenIndex).value.Length - 1
                            errorPointer = errorPointer & " "
                        Next
                    Next
                    errorMessage = "the name """ & organicName & """ is not valid" & Environment.NewLine _
                        & organicName & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & """" & possibleToken & """ is not valid (no matching tokens)"

                    Dim errorToken As Token
                    errorToken.type = "ERROR"
                    errorToken.value = errorMessage

                    Return {errorToken}

                Else
                    ' otherwise push lastHitToken to tokens and reset

                    ' add lastHitToken to tokens
                    ReDim Preserve tokens(tokens.Length)
                    tokens(tokens.Length - 1) = lastTokenHit

                    ' reset lastHitToken
                    lastTokenHit.type = ""
                    lastTokenHit.value = "-1"

                    ' reset possibleToken
                    possibleToken = ""

                    ' push index to first char after newly added token
                    index = indexAfterLastTokenHit
                    indexAfterLastTokenHit = -1
                End If
            End If
        End While


        ' step 2. verify token list

        ' check if all tokens present are allowed to follow the tokens they are following
        Dim currentToken As Token
        Dim currentTokenDefinition As TokenDefinition
        Dim nextToken As Token
        Dim nextTokenDefinition As TokenDefinition

        currentTokenDefinition.followers = {} ' get rid of annoying warning

        Dim fullAllowedNext As String()
        fullAllowedNext = {}

        For tokenIndex = 0 To tokens.Length - 2
            ' store current and next token and their tokenDefinitions
            currentToken = tokens(tokenIndex)
            nextToken = tokens(tokenIndex + 1)
            For tokenDefIndex = 0 To tokenDefinitions.Length - 1
                If tokenDefinitions(tokenDefIndex).name = currentToken.type Then
                    currentTokenDefinition = tokenDefinitions(tokenDefIndex)
                End If
                If tokenDefinitions(tokenDefIndex).name = nextToken.type Then
                    nextTokenDefinition = tokenDefinitions(tokenDefIndex)
                End If
            Next

            ' expand all special tokens with contained followers
            For allowedTokenFollower = 0 To currentTokenDefinition.followers.Length - 1
                ' find token definition of follower, check if needs unpacking and expand if it does
                For tokenDefIndex = 0 To tokenDefinitions.Length - 1
                    If tokenDefinitions(tokenDefIndex).name = currentTokenDefinition.followers(allowedTokenFollower) Then
                        If tokenDefinitions(tokenDefIndex).contained.Length = 0 Then
                            ' no need to unpack
                            ReDim Preserve fullAllowedNext(fullAllowedNext.Length)
                            fullAllowedNext(fullAllowedNext.Length - 1) = currentTokenDefinition.followers(allowedTokenFollower)
                        Else
                            ' unpack
                            fullAllowedNext = fullAllowedNext.Concat(tokenDefinitions(tokenDefIndex).contained).ToArray()
                        End If
                    End If
                Next
            Next


            If Not fullAllowedNext.Contains(nextToken.type) Then
                ' create error message and return early
                Dim errorMessage As String
                Dim errorPointer As String
                errorPointer = ""
                For tIndex = 0 To tokenIndex
                    For sIndex = 0 To tokens(tIndex).value.Length - 1
                        errorPointer = errorPointer & " "
                    Next
                Next
                errorMessage = "the name """ & organicName & """ is not valid" & Environment.NewLine _
                    & organicName & Environment.NewLine _
                    & errorPointer & "^" & Environment.NewLine _
                    & "a " & nextToken.type & " can't follow a " & currentToken.type

                Dim errorToken As Token
                errorToken.type = "ERROR"
                errorToken.value = errorMessage

                Return {errorToken}
            End If

        Next

        ' check if starting token of the name is allowed
        For tokenDef = 0 To tokenDefinitions.Length - 1
            ' nameStart is a special token that contains the tokens a name can start with
            If tokenDefinitions(tokenDef).name = "nameStart" Then
                ' if the first token isn't contained in the 'nameStart' token, create error message and return early
                If Not tokenDefinitions(tokenDef).contained.Contains(tokens(0).type) Then
                    Dim errorMessage As String
                    errorMessage = "the name """ & organicName & """ is not valid" & Environment.NewLine _
                        & organicName & Environment.NewLine _
                        & "^" & Environment.NewLine _
                        & "an organic name can't start with a " & tokens(0).type

                    Dim errorToken As Token
                    errorToken.type = "ERROR"
                    errorToken.value = errorMessage

                    Return {errorToken}
                End If
            End If
        Next

        Return tokens

    End Function

End Module
