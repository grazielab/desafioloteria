      *Divisão de identificação do programa
       identification division.
       program-id. "desafioloteria".
       author. "Graziela Bartyra Bressanini Beckhauser".
       installation. "PC".
       date-written. 25/07/2020.
       date-compiled. 15/08/2020.


      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  ws-sorteio.
           05  ws-semente                          pic  9(08).
           05  ws-semente1                         pic  9(08).
           05  ws-num_random                       pic  9(01)v9(08).

       01  ws-numeros-sorteados.
           05  ws-sort-num1                        pic  9(02).
           05  ws-sort-num2                        pic  9(02).
           05  ws-sort-num3                        pic  9(02).
           05  ws-sort-num4                        pic  9(02).
           05  ws-sort-num5                        pic  9(02).
           05  ws-sort-num6                        pic  9(02).

       01  ws-numeros-usuario.
           05  ws-num1                             pic  9(02).
           05  ws-num2                             pic  9(02).
           05  ws-num3                             pic  9(02).
           05  ws-num4                             pic  9(02).
           05  ws-num5                             pic  9(02).
           05  ws-num6                             pic  9(02).
           05  ws-num7                             pic  9(02).
           05  ws-num8                             pic  9(02).
           05  ws-num9                             pic  9(02).
           05  ws-num10                            pic  9(02).

       01  ws-uso-comum.
           05  ws-ind-lot                          pic  9(02).
           05  ws-ind                              pic  9(02).
           05  ws-qtd-num-jog                      pic  9(02).
           05  ws-tentativa                        pic  9(02).
           05  ws-contador                         pic  9(09).

       01  ws-relogio.
           05  ws-hora                             pic  9(02).
           05  ws-minuto                           pic  9(02).
           05  ws-segundo                          pic  9(02).
           05  ws-cent_segundo                     pic  9(02).

       77  ws-diferenca-hr                         pic  9(02).
       77  ws-diferenca-min                        pic  9(02).
       77  ws-diferenca-seg                        pic  9(02).
       01  ws-hora-inicio.
           05  ws-hor                              pic 9(002).
           05  ws-min                              pic 9(002).
           05  ws-seg                              pic 9(002).

       01  ws-hora-final.
           05  ws-hor-fim                          pic  9(02).
           05  ws-min-fim                          pic  9(02).
           05  ws-seg-fim                          pic  9(02).

      *----Variaveis para comunicação entre programas
       linkage section.

      *----Declaração de tela
       screen section.

      *Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>-------------------------------------------------
      *>     Processamento
      *>-------------------------------------------------

       processamento section.

        display "Informe o primeiro numero: "
        accept ws-num1

        display "Informe o segundo numero: "
        accept ws-num2

        display "Informe o terceiro numero: "
        accept ws-num3

        display 'Informe o Quarto Numero?'
        accept ws-num4

        display "Informe o terceiro numero: "
        accept ws-num5

        display "Informe o terceiro numero: "
        accept ws-num6

        display "Informe o terceiro numero: "
        accept ws-num7

        display "Informe o terceiro numero: "
        accept ws-num8

        display "Informe o terceiro numero: "
        accept ws-num9

        display "Informe o terceiro numero: "
        accept ws-num10

        perform sorteio
             .

       processamento-exit.
           exit.


      *>------------------------------------------------------------------
      *> Realização do Sorteio em formato randomico
      *>------------------------------------------------------------------

       sorteio section.

           move zero to ws-ind-lot

           perform until ws-ind-lot <> 0

           move ws-semente   to ws-relogio

           accept ws-semente from time

               perform semente-delay
               compute ws-sort-num1 = function random(ws-semente) * 60

               perform semente-delay
               compute ws-sort-num2 = function random(ws-semente) * 60

               perform semente-delay
               compute ws-sort-num3 = function random(ws-semente) * 60

               perform semente-delay
               compute ws-sort-num4 = function random(ws-semente) * 60

               perform semente-delay
               compute ws-sort-num5 = function random(ws-semente) * 60

               perform semente-delay
               compute ws-sort-num6 = function random(ws-semente) * 60

                perform confere-num-sorteados

            end-perform
             .

       sorteio-exit.
           exit.

      *>-----------------------
      *> Delay
      *>-----------------------

       semente-delay section.

       perform 10 times

           accept ws-semente1 from time
           move ws-semente1    to ws-semente
           perform until ws-semente > ws-semente1
               accept ws-semente from time

           end-perform

       end-perform
                .

       semente-delay-exit.

      *>---------------------------------------------------------------------------------
      *> Conferindo se os números são diferentes entre eles e diferentes de 00
      *>---------------------------------------------------------------------------------

       confere-num-sorteados section.

           if   ws-sort-num1 <> ws-sort-num2
           and  ws-sort-num1 <> ws-sort-num3
           and  ws-sort-num1 <> ws-sort-num4
           and  ws-sort-num1 <> ws-sort-num5
           and  ws-sort-num1 <> ws-sort-num6

              if   ws-sort-num2 <> ws-sort-num3
              and  ws-sort-num2 <> ws-sort-num4
              and  ws-sort-num2 <> ws-sort-num5
              and  ws-sort-num2 <> ws-sort-num6

                 if   ws-sort-num3 <> ws-sort-num4
                  and  ws-sort-num3 <> ws-sort-num5
                  and  ws-sort-num3 <> ws-sort-num6
                      if   ws-sort-num4 <> ws-sort-num5
                      and  ws-sort-num4 <> ws-sort-num6
                         if   ws-sort-num5 <> ws-sort-num6


                               perform 2400-conferir-aposta
                               display ws-sort-num1 ' | ' ws-sort-num2
                               ' | ' ws-sort-num3 ' | ' ws-sort-num4 ' | '
                               ws-sort-num5 ' | ' ws-sort-num6 ' | '
                               ' - ' ws-contador

                           else
                                perform sorteio
                           end-if
                       end-if
                   end-if
                end-if
             end-if
               .

           confere-num-sorteados-exit.
               exit.

      *>-----------------------------------------------------------------
      *>Conferindo se números sorteados são iguais ao da aposta
      *>-----------------------------------------------------------------

        confere-aposta section.

          add 1 to ws-contador

          if   ws-sort-num1 = ws-num1 or ws-sort-num1 = ws-num2
          or   ws-sort-num1 = ws-num3 or ws-sort-num1 = ws-num4
          or   ws-sort-num1 = ws-num5 or ws-sort-num1 = ws-num6
          or   ws-sort-num1 = ws-num7 or ws-sort-num1 = ws-num8
          or   ws-sort-num1 = ws-num9 or ws-sort-num1 = ws-num10 then

               if   ws-sort-num2 = ws-num1 or ws-sort-num2 = ws-num2
               or   ws-sort-num2 = ws-num3 or ws-sort-num2 = ws-num4
               or   ws-sort-num2 = ws-num5 or ws-sort-num2 = ws-num6
               or   ws-sort-num2 = ws-num7 or ws-sort-num2 = ws-num8
               or   ws-sort-num2 = ws-num9 or ws-sort-num2 = ws-num10 then

                   if   ws-sort-num3 = ws-num1 or ws-sort-num3 = ws-num2
                   or   ws-sort-num3 = ws-num3 or ws-sort-num3 = ws-num4
                   or   ws-sort-num3 = ws-num5 or ws-sort-num3 = ws-num6
                   or   ws-sort-num3 = ws-num7 or ws-sort-num3 = ws-num8
                   or   ws-sort-num3 = ws-num9 or ws-sort-num3 = ws-num10 then

                        if   ws-sort-num4 = ws-num1 or ws-sort-num4 = ws-num2
                        or   ws-sort-num4 = ws-num3 or ws-sort-num4 = ws-num4
                        or   ws-sort-num4 = ws-num5 or ws-sort-num4 = ws-num6
                        or   ws-sort-num4 = ws-num7 or ws-sort-num4 = ws-num8
                        or   ws-sort-num4 = ws-num9 or ws-sort-num4 = ws-num10 then

                             if   ws-sort-num5 = ws-num1 or ws-sort-num5 = ws-num2
                             or   ws-sort-num5 = ws-num3 or ws-sort-num5 = ws-num4
                             or   ws-sort-num5 = ws-num5 or ws-sort-num5 = ws-num6
                             or   ws-sort-num5 = ws-num7 or ws-sort-num5 = ws-num8
                             or   ws-sort-num5 = ws-num9 or ws-sort-num5 = ws-num10 then

                                  if   ws-sort-num6 = ws-num1 or ws-sort-num6 = ws-num2
                                  or   ws-sort-num6 = ws-num3 or ws-sort-num6 = ws-num4
                                  or   ws-sort-num6 = ws-num5 or ws-sort-num6 = ws-num6
                                  or   ws-sort-num6 = ws-num7 or ws-sort-num6 = ws-num8
                                  or   ws-sort-num6 = ws-num9 or ws-sort-num6 = ws-num10 then

                                       move function current-date(9:6)  to  ws-hora-final
                                       display "Os numeros estao corretos, parabens!"
                                       display ws-num1 " - " ws-num2 " - " ws-num3 " - "
                                       ws-num4 " - " ws-num5 " - " ws-num6 " - " ws-num7
                                       " - " ws-num8 " - " ws-num9 " - " ws-num10 " - "

                                       perform temp-sorteando

                                       display 'Tempo que levou para acertar' ws-diferenca-hr ' : '
                                       ws-diferenca-min ' : ' ws-diferenca-seg

                                       display 'Quantidade de Sorteios - ' ws-contador

                                       perform finaliza
                                  else
                                       perform sorteio
                                 end-if
                             end-if
                         end-if
                     end-if
                end-if
           end-if
            .

         confere-aposta-exit.
             exit.
      *>---------------------------------------------------------------------------------
      *> Tempo que os números ficaram sorteando
      *>---------------------------------------------------------------------------------

       temp-sorteando section.
           compute ws-diferenca-hr  = (ws-hor - ws-hor-fim)
           compute ws-diferenca-min = (ws-min - ws-min-fim)
           compute ws-diferenca-seg = (ws-seg - ws-seg-fim)
             .

       temp-sorteando-exit.
           exit.

      *>---------------------------------
      *> Finalização
      *>---------------------------------

       finaliza section.
           stop run.
              .
       finaliza-exit.
           exit.





