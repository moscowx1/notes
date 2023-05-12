import { useForm } from "react-hook-form";

interface IForm {
  login: string;
  password: string;
}

const Form = () => {
  const { register, handleSubmit } = useForm<IForm>();
  const onSubmit = (data: IForm) => console.log(data);

  return (
    <>
      <form onSubmit={handleSubmit(onSubmit)}>
        <input {...register('login')}/>
        <input type="password" {...register('password')}/>
      </form>
    </>
  )
}

export default Form;