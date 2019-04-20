Class MainWindow 

    Private Sub Window_Loaded(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles MyBase.Loaded

    End Sub

    Private Sub testParsing(ByVal nameToTest As String)
        ' load token definitions from token definitions Xml file
        Dim tokenDefinitionsXML As XElement
        tokenDefinitionsXML = XElement.Load(OCVresources.tokenDefinitionsFile)

        ' create token definition list
        Dim tokenDefinitions As parseIUPACName.TokenDefinition()
        tokenDefinitions = parseIUPACName.createTokenDefinitons(tokenDefinitionsXML)

        ' tokenise example name
        Console.WriteLine("parsing " & nameToTest)
        Console.WriteLine()
        Dim result = parseIUPACName.tokenise(nameToTest, tokenDefinitions)
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
