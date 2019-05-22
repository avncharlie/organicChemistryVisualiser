Module OCVresources
    Public startupFolder As String = System.AppDomain.CurrentDomain.BaseDirectory & "resources\"
    Public tokenDefinitionsFile As String = startupFolder & "tokens.xml"
    Public functionalGroupDefinitionsFile As String = startupFolder & "functionalGroups.xml"
    Public iconPath As String = startupFolder & "carbonRings.ico"
    Public examplesPath As String = startupFolder & "examples.xml"

    Public scale As Double = 1

    Public alkaneSpacing As Double = 30
    Public alkaneRise As Double = 17

    Public alkeEneYneLineXOffsetPercentage As Double = 3 / 30   ' 0.1
    Public alkEneYneLineYOffsetPercentage As Double = 5 / 17    ' 0.294117...

    Public canvasOffset As Double() = {30, 30}

    Public alkaneStartsRising As Boolean = False

    ' sorted from first preference to last (1 - 6)
    ' assumed from 0 degrees being east and pi/2 degrees being north
    Public branchAngles As Double() = {
        Math.PI / 2,
        (11 * Math.PI) / 6,
        (7 * Math.PI) / 6,
        Math.PI / 6,
        (5 * Math.PI) / 6,
        (3 * Math.PI) / 2
    }
End Module
