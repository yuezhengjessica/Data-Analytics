number=input('please enter a measurement:')
if 'm' in number:
  number1=eval(number[0:-1])/0.0254
  print('{:.3f}in'.format(number1))
elif 'in' in number:
  number2=eval(number[0:-2])*0.0254
  print('{:.3f}m'.format(number2))
