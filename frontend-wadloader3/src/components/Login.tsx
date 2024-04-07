import { Button, FormControl, TextField } from "@mui/material";

export function Login() {
    return (
        <FormControl>
            <TextField id="useranme" />
            <TextField id="passwrod" type="password" />
            <Button onClick={() => {
                
            }}>Login</Button>
        </FormControl>
    );
}