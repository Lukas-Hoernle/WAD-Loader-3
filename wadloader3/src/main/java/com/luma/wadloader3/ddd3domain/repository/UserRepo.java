package com.luma.wadloader3.ddd3domain.repository;



import com.luma.wadloader3.ddd3domain.model.User;

import java.util.List;
import java.util.Optional;

public interface UserRepo {
    Optional<User> findByUsername(String name);
    boolean existsByUsernameAndPassword(String name, String password);
    boolean existsByUsername(String name);
    List<User> findAll();
    void deleteById(String name);
    void delete(User user);
    User saveAndFlush(User user);


}
