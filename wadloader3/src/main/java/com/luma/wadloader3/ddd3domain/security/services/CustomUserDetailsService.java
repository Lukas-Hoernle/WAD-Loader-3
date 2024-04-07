package com.luma.wadloader3.ddd3domain.security.services;

import com.luma.wadloader3.ddd3domain.model.User;
import com.luma.wadloader3.ddd3domain.repository.UserRepo;
import com.luma.wadloader3.ddd3domain.security.model.CustomUserDetails;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepo userRepo;


    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        Optional<User> user = userRepo.findByUsername(username);
        if (user.isEmpty()) {
            throw new RuntimeException("User does not Exist");
        }
        return new CustomUserDetails(user.get());
    }
}
