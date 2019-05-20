Class MainWindow 
    Dim c_display As New Canvas
    Dim tokenDefinitions As IUPACParser.TokenDefinition()
    Dim functionalGroupDefinitions As IUPACParser.FunctionalGroupDefinition()

    Private Sub Window_Loaded(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles MyBase.Loaded
        ' set starting element to be something (stub)
        textBox_userInput.FontStyle = FontStyles.Normal
        textBox_userInput.Foreground = New SolidColorBrush(Colors.Black)
        textBox_userInput.Text = "1-(6-carboxy-5-nitro-2-bromo-hex-3-ynyl)cyclodecane"
        textBox_userInput.Text = "1-cyclopropyl-2-cyclobutyl-3-cyclopentyl-4-cyclohexyl-5-cycloheptyl-6-cyclooctyl-7-cyclononyl-8-cyclodecyl-9-cycloundecyl-10-cyclododecyl-cyclodecane"
        'textBox_userInput.Text = "1-cyclohexyldecane"

        ' generate token and functional group definitions
        tokenDefinitions = IUPACParser.generateTokenDefinitions(XElement.Load(OCVresources.tokenDefinitionsFile))
        functionalGroupDefinitions = IUPACParser.generateFunctionalGroupDefinitions(XElement.Load(OCVresources.functionalGroupDefinitionsFile))
    End Sub

    ''' <summary>
    ''' Given an organicName, uses the functions and subroutines present in IUPACParser and IUPACRenderer to display either an error for an incorrect name
    ''' or a rendered picture of the compound
    ''' </summary>
    ''' <param name="organicName">A string containing the name to be rendered</param>
    ''' <remarks></remarks>
    Private Sub parseAndDisplay(ByVal organicName As String)
        ' wipe canvas on new input
        c_display.Children.Clear()

        ' tokenise example name
        Dim tokens As Token()
        tokens = IUPACParser.generateTokens(organicName, tokenDefinitions)

        ' display error if there is one
        If tokens(0).type = "ERROR" Then
            tb_error.Text = tokens(0).value
        Else
            ' convert tokens to AST
            Dim AST As IUPACParser.ASTRoot
            AST = IUPACParser.generateAST(organicName, tokens, tokenDefinitions)

            ' display error if there is one, otherwise render
            If AST.compoundTree.isError Then
                tb_error.Text = AST.compoundTree.errorMessage
            Else
                ' remove previously displayed AST
                If c_containerDisplay.Children.Contains(c_display) Then
                    c_containerDisplay.Children.Remove(c_display)
                End If

                ' remove error message
                tb_error.Text = ""

                ' display rendered AST
                c_display = IUPACRenderer.renderAST(AST.compoundTree, _
                                                    functionalGroupDefinitions, _
                                                    {c_containerDisplay.ActualWidth, c_containerDisplay.ActualHeight}, _
                                                    OCVresources.scale, _
                                                    OCVresources.alkaneSpacing, OCVresources.alkaneRise, _
                                                    OCVresources.canvasOffset(0), OCVresources.canvasOffset(1), _
                                                    OCVresources.alkaneStartsRising, OCVresources.alkeEneYneLineXOffsetPercentage, _
                                                    OCVresources.alkEneYneLineYOffsetPercentage,
                                                    True).canvas
                c_containerDisplay.Children.Add(c_display)

                ' render image (stub)
                IUPACRenderer.exportCanvasAsImage(c_display, "png", "\\Mac\Home\Desktop\output.png")
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
        If textBox_userInput.Text = "" Then
            textBox_userInput.FontStyle = FontStyles.Italic
            textBox_userInput.Foreground = New SolidColorBrush(ColorConverter.ConvertFromString("#9b9b9b"))
            textBox_userInput.Text = "Enter IUPAC name"
        End If
    End Sub

    ''' <summary>
    ''' Removes the placeholder text and all styling from it for normal use when clicked on by user.
    ''' </summary>
    ''' <param name="sender">TextBox - sent automatically</param>
    ''' <param name="e">System.Windows.RoutedEventArgs - sent automatically</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_removePlaceHolderText(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles textBox_userInput.GotFocus
        If textBox_userInput.Text = "Enter IUPAC name" Then
            textBox_userInput.FontStyle = FontStyles.Normal
            textBox_userInput.Foreground = New SolidColorBrush(Colors.Black)
            textBox_userInput.Text = ""
        End If
    End Sub

    ''' <summary>
    ''' When a name is entered to textBox_userInput and the user presses enter, this subroutine calls parseAndDisplay with the entered name
    ''' </summary>
    ''' <param name="sender">Handled by system - will be textBox_userInput</param>
    ''' <param name="e">Handled by system - will a System.Windows.Input.KeyEventArgs with the key a user entered (keys that are not enter are ignored)</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles textBox_userInput.KeyDown
        If e.Key = Key.Enter Then
            parseAndDisplay(textBox_userInput.Text)
        End If
    End Sub
End Class
