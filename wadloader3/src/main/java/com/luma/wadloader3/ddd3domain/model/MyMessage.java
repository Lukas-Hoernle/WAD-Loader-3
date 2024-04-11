package com.luma.wadloader3.ddd3domain.model;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "MyMessage")
public class MyMessage {

    @Id
    @GeneratedValue
    private int id;

    @Column(name = "message")
    private String message;
}
