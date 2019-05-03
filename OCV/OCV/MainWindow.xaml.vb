Class MainWindow 

    Private Sub Window_Loaded(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles MyBase.Loaded

    End Sub

    Private Sub testParsing(ByVal nameToTest As String)
        ' load token definitions from token definitions XML file and create token definition list
        Dim tokenDefinitionsXML As XElement
        Dim tokenDefinitions As IUPACParser.TokenDefinition()
        tokenDefinitionsXML = XElement.Load(OCVresources.tokenDefinitionsFile)
        tokenDefinitions = IUPACParser.generateTokenDefinitions(tokenDefinitionsXML)

        ' tokenise example name
        Console.WriteLine("tokenising " & nameToTest)
        Dim tokens As Token()
        tokens = IUPACParser.generateTokens(nameToTest, tokenDefinitions)

        ' display error if there is one
        If tokens(0).type = "ERROR" Then
            Console.WriteLine(tokens(0).value)
        Else
            ' convert tokens to AST
            Console.WriteLine("generating AST for " & nameToTest)
            Dim AST As IUPACParser.ASTRoot
            AST = IUPACParser.generateAST(nameToTest, tokens, tokenDefinitions)

            If AST.compoundTree.isError Then
                Console.WriteLine(AST.compoundTree.errorMessage)
            End If

            Console.WriteLine()
        End If


    End Sub

    ''' <summary>
    ''' Sets the placeholder text 'Enter IUPAC name' to the user input textbox (textBox_userInput) when it loses focus. Also makes the text italic and gray.
    ''' </summary>
    ''' <param name="sender">TextBox - sent automatically</param>
    ''' <param name="e">System.Windows.RoutedEventArgs - sent automatically</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_setPlaceHolderText(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles textBox_userInput.LostFocus
        textBox_userInput.FontStyle = FontStyles.Italic
        textBox_userInput.Foreground = New SolidColorBrush(ColorConverter.ConvertFromString("#9b9b9b"))
        textBox_userInput.Text = "Enter IUPAC name"
    End Sub

    ''' <summary>
    ''' Removes the placeholder text and all styling from it for normal use when clicked on by user.
    ''' </summary>
    ''' <param name="sender">TextBox - sent automatically</param>
    ''' <param name="e">System.Windows.RoutedEventArgs - sent automatically</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_removePlaceHolderText(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles textBox_userInput.GotFocus
        textBox_userInput.FontStyle = FontStyles.Normal
        textBox_userInput.Foreground = New SolidColorBrush(Colors.Black)
        textBox_userInput.Text = ""
    End Sub

    ' create token list
    ' parse token list into AST
    ' parse AST into OrganicCompound

    ' parseIUPACName module 
    ' parseIUPACName.parse(name) = OrganicCompound
    ' parseIUPACName.parse contains parseIUPACName.tokenise, parseIUPACName.tokensToAST and parseIUPACName.AST2OrganicCompound

    Private Sub textBox_userInput_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles textBox_userInput.KeyDown
        If e.Key = Key.Enter Then
            testParsing(textBox_userInput.Text)
        End If
    End Sub


End Class
