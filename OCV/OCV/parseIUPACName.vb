Imports System.Text.RegularExpressions

Module parseIUPACName

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

    ''' <summary>
    ''' Given a singular token definition as an XML node and all the token definitions in XML format, return a TokenDefinition
    ''' structure representing the token definition. Handles unpacking regexes and expanding contained tokens within other tokens
    ''' recursively.
    ''' </summary>
    ''' <param name="tokenDefinitionXML">An XElement containing the token definition to be converted</param>
    ''' <param name="allTokenDefinitionsXML">An XElement containing all token definitions (used when tokenDefinitionXML contains other tokens)</param>
    ''' <returns>Returns a TokenDefinition representing tokenDefinitionXML</returns>
    ''' <remarks>Works for recursive definitions and expanded regexes.</remarks>
    Private Function createTokenDefinition(ByVal tokenDefinitionXML As XElement, ByVal allTokenDefinitionsXML As XElement) As TokenDefinition
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
                tokenDefContained = createTokenDefinition(containedTokens(index), allTokenDefinitionsXML)
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
    ''' </summary>
    ''' <param name="allTokenDefinitionsXML">An XElement containing all tokens to be converted into a TokenDefinitions</param>
    ''' <returns>An array of TokenDefinitions</returns>
    ''' <remarks>Uses the createTokenDefinition function to create singular TokenDefinitions</remarks>
    Public Function createTokenDefinitons(ByVal allTokenDefinitionsXML As XElement) As TokenDefinition()
        ' created token definitions array
        Dim tokenDefinitions As TokenDefinition()
        ReDim tokenDefinitions(allTokenDefinitionsXML.Elements().Count - 1)

        ' create individual tokenDefinitions and add to array
        Dim counter As Integer
        counter = 0
        For Each tokenDefinitionXML In allTokenDefinitionsXML.Elements()
            tokenDefinitions(counter) = createTokenDefinition(tokenDefinitionXML, allTokenDefinitionsXML)
            counter = counter + 1
        Next

        Return tokenDefinitions
    End Function

    ''' <summary>
    ''' Given an organicName and a tokenDefinition list, tokenise it, meaning convert the organic name into an array of Token structures.
    ''' The token array is then verified. If any error is encountered, returns the error in the form of an array of a single Token where
    ''' type="ERROR" and value contains the error details.
    ''' </summary>
    ''' <param name="organicName">A string containing the organicName to be tokenised</param>
    ''' <param name="tokenDefinitions">An array of tokenDefinitions used to tokenise the name</param>
    ''' <returns>An array of Token structures representing the organic name OR an array containing a single token whose type is ERROR and value contains an error message.</returns>
    ''' <remarks>The createTokenDefinitions function can be used to generate tokenDefinitions</remarks>
    Public Function tokenise(ByVal organicName As String, ByVal tokenDefinitions As TokenDefinition()) As Token()
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
                    errorMessage = "ERROR" & Environment.NewLine _
                        & organicName & Environment.NewLine _
                        & errorPointer & "^" & Environment.NewLine _
                        & """" & possibleToken & """ is not valid (no matching token)"

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

        Dim currentToken As Token
        Dim currentTokenDefinition As TokenDefinition
        Dim nextToken As Token
        Dim nextTokenDefinition As TokenDefinition

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
                errorMessage = "ERROR" & Environment.NewLine _
                    & organicName & Environment.NewLine _
                    & errorPointer & "^" & Environment.NewLine _
                    & "a " & nextToken.type & " can't follow a " & currentToken.type

                Dim errorToken As Token
                errorToken.type = "ERROR"
                errorToken.value = errorMessage

                Return {errorToken}
            End If

        Next

        Return tokens

    End Function

End Module
