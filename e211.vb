Enter file contents hereSub Init()
   Dim I As Integer, J As Integer
   Dim WS As Worksheet
   Set WS = Sheets("Sheet1")
   
   Application.ScreenUpdating = False
   For I = 0 To 29
      For J = 0 To 29
         WS.Range("D3").Offset(I, J).Value = 1
         WS.Range("AT3").Offset(I, J).Value = 0
      Next J
   Next I
   
   WS.Range("AD38:AH38").Value = vbNullString
   WS.Range("AE39").Value = vbNullString
   
   Application.ScreenUpdating = True
End Sub

Sub RunSample()
   Dim I As Integer, J As Integer, iter As Integer
   Dim Temp As Double, ii As Integer, jj As Integer
   Dim WS As Worksheet
   Set WS = Sheets("Sheet1")
   
   Init
   
   For iter = 1 To 50
      Application.ScreenUpdating = False
      ' set new values
      WS.Range("AT3:BW32").Value = 0
      
      For I = 0 To 29
         For J = 0 To 29
            Temp = 0
            ' add up the legal moves
            For ii = -1 To 1
               For jj = -1 To 1
                  If (Abs(ii) <> Abs(jj)) Then
                     If (LegalMove(I + ii, J + jj, 30)) Then
                        Temp = Temp + 1
                     End If
                  End If
               Next jj
            Next ii
            
            ' add them all up
            For ii = -1 To 1
               For jj = -1 To 1
                  If (Abs(ii) <> Abs(jj)) Then
                     If (LegalMove(I + ii, J + jj, 30)) Then
                        WS.Range("AT3").Offset(I + ii, J + jj).Value = WS.Range("AT3").Offset(I + ii, J + jj).Value + WS.Range("D3").Offset(I, J).Value / Temp
                     End If
                  End If
               Next jj
            Next ii
            
            
         Next J
      Next I
      
      ' copy new values over
      For I = 0 To 29
         For J = 0 To 29
            WS.Range("D3").Offset(I, J).Value = WS.Range("AT3").Offset(I, J).Value
         Next J
      Next I
      Application.ScreenUpdating = True
   Next iter
   
   ' process results
   Temp = 0
   For I = 0 To 29
      For J = 0 To 29
         Temp = Temp + iMax(0, 1 - WS.Range("D3").Offset(I, J).Value)
      Next J
   Next I
   WS.Range("AE39").Value = Temp
End Sub


Function iMax(A As Double, B As Double) As Double
   If (A > B) Then
      iMax = A
   Else
      iMax = B
   End If
End Function

Function LegalMove(I As Integer, J As Integer, S As Integer) As Boolean
   LegalMove = ((I >= 0) And (I <= S - 1) And (J >= 0) And (J <= S - 1))
End Function


Function Odds(I As Integer, J As Integer, S As Integer) As Double
   If IsCorner(I, J, S) Then
      Odds = 1 / 2
   ElseIf IsEdge(I, J, S) Then
      Odds = 1 / 3
   Else
      Odds = 1 / 4
   End If
End Function


Function IsCorner(I As Integer, J As Integer, S As Integer) As Boolean
   IsCorner = (((I = 0) Or (I = S - 1)) And ((J = 0) Or (J = S - 1)))
End Function


Function IsEdge(I As Integer, J As Integer, S As Integer) As Boolean
   IsEdge = (((I = 0) Or (I = S - 1)) Or ((J = 0) Or (J = S - 1)))
End Function
