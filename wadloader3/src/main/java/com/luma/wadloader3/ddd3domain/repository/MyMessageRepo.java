package com.luma.wadloader3.ddd3domain.repository;

import com.luma.wadloader3.ddd3domain.model.MyMessage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Indexed;
import org.springframework.stereotype.Repository;

@Repository
public interface MyMessageRepo extends JpaRepository<MyMessage, Integer> {
    MyMessage findMyMessageByMessage(String message);
}
